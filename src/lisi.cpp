#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>
#include <thread>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace Rcpp;

namespace {

inline bool is_valid_distance(double x) {
  return !NumericVector::is_na(x) && std::isfinite(x);
}

struct HBetaResult {
  double entropy;
  std::vector<double> probs;
};

struct KnnEntry {
  double dist_sq;
  int index;
};

inline bool knn_entry_less(const KnnEntry& a, const KnnEntry& b) {
  if (a.dist_sq < b.dist_sq) {
    return true;
  }
  if (a.dist_sq > b.dist_sq) {
    return false;
  }
  return a.index < b.index;
}

struct ExactKnnWorker {
  const double* x;
  int* nn_idx;
  double* nn_dists;
  int n;
  int p;
  int k;
  bool exclude_self;

  ExactKnnWorker(
    const double* x,
    int* nn_idx,
    double* nn_dists,
    int n,
    int p,
    int k,
    bool exclude_self
  ) : x(x),
      nn_idx(nn_idx),
      nn_dists(nn_dists),
      n(n),
      p(p),
      k(k),
      exclude_self(exclude_self) {}

  inline double at(int row, int col) const {
    return x[row + n * col];
  }

  void run(std::size_t begin, std::size_t end) const {
    std::vector<KnnEntry> dist_idx;
    dist_idx.reserve(static_cast<std::size_t>(n));

    for (std::size_t i_size = begin; i_size < end; ++i_size) {
      const int i = static_cast<int>(i_size);
      dist_idx.clear();

      for (int j = 0; j < n; ++j) {
        if (exclude_self && i == j) {
          continue;
        }

        double dist_sq = 0.0;
        for (int col = 0; col < p; ++col) {
          const double diff = at(i, col) - at(j, col);
          dist_sq += diff * diff;
        }

        dist_idx.push_back(KnnEntry{dist_sq, j + 1});
      }

      std::partial_sort(
        dist_idx.begin(),
        dist_idx.begin() + k,
        dist_idx.end(),
        knn_entry_less
      );

      for (int neighbor = 0; neighbor < k; ++neighbor) {
        nn_idx[i + n * neighbor] = dist_idx[neighbor].index;
        nn_dists[i + n * neighbor] = std::sqrt(std::max(0.0, dist_idx[neighbor].dist_sq));
      }
    }
  }
};

HBetaResult hbeta_cpp(const NumericMatrix& D, int cell, double beta) {
  const int k = D.nrow();
  std::vector<double> probs(k, 0.0);
  double sum_probs = 0.0;
  double weighted_sum = 0.0;

  for (int neighbor = 0; neighbor < k; ++neighbor) {
    const double dist = D(neighbor, cell);
    if (!is_valid_distance(dist)) {
      continue;
    }

    const double prob = std::exp(-dist * beta);
    probs[neighbor] = prob;
    sum_probs += prob;
    weighted_sum += dist * prob;
  }

  if (!(sum_probs > 0.0) || !std::isfinite(sum_probs)) {
    return HBetaResult{0.0, std::vector<double>(k, 0.0)};
  }

  for (int neighbor = 0; neighbor < k; ++neighbor) {
    probs[neighbor] /= sum_probs;
  }

  const double entropy = std::log(sum_probs) + beta * weighted_sum / sum_probs;
  return HBetaResult{entropy, probs};
}

HBetaResult hbeta_knn_entries_cpp(
    const std::vector<KnnEntry>& entries,
    int k,
    double beta) {
  std::vector<double> probs(k, 0.0);
  double sum_probs = 0.0;
  double weighted_sum = 0.0;

  for (int neighbor = 0; neighbor < k; ++neighbor) {
    const double dist = std::sqrt(std::max(0.0, entries[neighbor].dist_sq));
    const double prob = std::exp(-dist * beta);
    probs[neighbor] = prob;
    sum_probs += prob;
    weighted_sum += dist * prob;
  }

  if (!(sum_probs > 0.0) || !std::isfinite(sum_probs)) {
    return HBetaResult{0.0, std::vector<double>(k, 0.0)};
  }

  for (int neighbor = 0; neighbor < k; ++neighbor) {
    probs[neighbor] /= sum_probs;
  }

  const double entropy = std::log(sum_probs) + beta * weighted_sum / sum_probs;
  return HBetaResult{entropy, probs};
}

std::vector<double> find_neighbor_probs_cpp(
    const NumericMatrix& D,
    int cell,
    double perplexity,
    double tol,
    int max_iter) {
  const double target_entropy = std::log(perplexity);
  double beta = 1.0;
  double beta_min = -std::numeric_limits<double>::infinity();
  double beta_max = std::numeric_limits<double>::infinity();

  HBetaResult hb = hbeta_cpp(D, cell, beta);
  double entropy_diff = hb.entropy - target_entropy;
  int tries = 0;

  while (std::abs(entropy_diff) > tol && tries < max_iter) {
    if (entropy_diff > 0.0) {
      beta_min = beta;
      beta = std::isfinite(beta_max) ? (beta + beta_max) / 2.0 : beta * 2.0;
    } else {
      beta_max = beta;
      beta = std::isfinite(beta_min) ? (beta + beta_min) / 2.0 : beta / 2.0;
    }

    hb = hbeta_cpp(D, cell, beta);
    entropy_diff = hb.entropy - target_entropy;
    ++tries;
  }

  return hb.probs;
}

std::vector<double> find_neighbor_probs_entries_cpp(
    const std::vector<KnnEntry>& entries,
    int k,
    double perplexity,
    double tol,
    int max_iter) {
  const double target_entropy = std::log(perplexity);
  double beta = 1.0;
  double beta_min = -std::numeric_limits<double>::infinity();
  double beta_max = std::numeric_limits<double>::infinity();

  HBetaResult hb = hbeta_knn_entries_cpp(entries, k, beta);
  double entropy_diff = hb.entropy - target_entropy;
  int tries = 0;

  while (std::abs(entropy_diff) > tol && tries < max_iter) {
    if (entropy_diff > 0.0) {
      beta_min = beta;
      beta = std::isfinite(beta_max) ? (beta + beta_max) / 2.0 : beta * 2.0;
    } else {
      beta_max = beta;
      beta = std::isfinite(beta_min) ? (beta + beta_min) / 2.0 : beta / 2.0;
    }

    hb = hbeta_knn_entries_cpp(entries, k, beta);
    entropy_diff = hb.entropy - target_entropy;
    ++tries;
  }

  return hb.probs;
}

struct LisiMatrixWorker {
  const double* x;
  const int* batch_labels;
  double* out;
  int n;
  int p;
  int k;
  int n_labels;
  int max_label;
  double perplexity;
  double tol;
  int max_iter;

  LisiMatrixWorker(
    const double* x,
    const int* batch_labels,
    double* out,
    int n,
    int p,
    int k,
    int n_labels,
    int max_label,
    double perplexity,
    double tol,
    int max_iter
  ) : x(x),
      batch_labels(batch_labels),
      out(out),
      n(n),
      p(p),
      k(k),
      n_labels(n_labels),
      max_label(max_label),
      perplexity(perplexity),
      tol(tol),
      max_iter(max_iter) {}

  inline double at(int row, int col) const {
    return x[row + n * col];
  }

  inline int label_at(int row, int label_col) const {
    return batch_labels[row + n * label_col];
  }

  void run(std::size_t begin, std::size_t end) const {
    std::vector<KnnEntry> dist_idx;
    dist_idx.reserve(static_cast<std::size_t>(n - 1));
    std::vector<double> label_sums(static_cast<std::size_t>(max_label + 1), 0.0);
    std::vector<int> touched;
    touched.reserve(max_label > 0 ? max_label : 1);

    for (std::size_t i_size = begin; i_size < end; ++i_size) {
      const int cell = static_cast<int>(i_size);
      dist_idx.clear();

      for (int other = 0; other < n; ++other) {
        if (cell == other) {
          continue;
        }

        double dist_sq = 0.0;
        for (int col = 0; col < p; ++col) {
          const double diff = at(cell, col) - at(other, col);
          dist_sq += diff * diff;
        }
        dist_idx.push_back(KnnEntry{dist_sq, other + 1});
      }

      std::partial_sort(
        dist_idx.begin(),
        dist_idx.begin() + k,
        dist_idx.end(),
        knn_entry_less
      );

      std::vector<double> probs = find_neighbor_probs_entries_cpp(
        dist_idx,
        k,
        perplexity,
        tol,
        max_iter
      );

      bool has_positive_prob = false;
      for (double prob : probs) {
        if (prob > 0.0) {
          has_positive_prob = true;
          break;
        }
      }
      if (!has_positive_prob) {
        continue;
      }

      for (int label_col = 0; label_col < n_labels; ++label_col) {
        touched.clear();
        for (int neighbor = 0; neighbor < k; ++neighbor) {
          const double prob = probs[neighbor];
          if (prob <= 0.0 || !std::isfinite(prob)) {
            continue;
          }

          const int idx = dist_idx[neighbor].index;
          if (idx < 1 || idx > n) {
            continue;
          }

          const int label = label_at(idx - 1, label_col);
          if (label < 1) {
            continue;
          }

          if (label >= static_cast<int>(label_sums.size())) {
            label_sums.resize(static_cast<std::size_t>(label + 1), 0.0);
          }
          if (label_sums[label] == 0.0) {
            touched.push_back(label);
          }
          label_sums[label] += prob;
        }

        if (touched.empty()) {
          continue;
        }

        double simpson = 0.0;
        for (int label : touched) {
          const double sum_prob = label_sums[label];
          simpson += sum_prob * sum_prob;
          label_sums[label] = 0.0;
        }
        out[cell + n * label_col] = simpson > 0.0 ? 1.0 / simpson : NA_REAL;
      }
    }
  }
};

} // namespace

// [[Rcpp::export]]
NumericVector compute_simpson_index_cpp(
    const NumericMatrix& D,
    const IntegerMatrix& knn_idx,
    const IntegerVector& batch_labels,
    double perplexity = 15,
    double tol = 1e-5,
    int max_iter = 50) {
  const int k = D.nrow();
  const int n_cells = D.ncol();

  if (knn_idx.nrow() != k || knn_idx.ncol() != n_cells) {
    stop("D and knn_idx must have the same dimensions.");
  }
  if (batch_labels.size() != n_cells) {
    stop("length(batch_labels) must equal ncol(D).");
  }
  if (!std::isfinite(perplexity) || perplexity <= 0.0) {
    stop("perplexity must be a single positive number.");
  }
  if (!std::isfinite(tol) || tol <= 0.0) {
    stop("tol must be a single positive number.");
  }
  if (max_iter < 1) {
    stop("max_iter must be at least 1.");
  }

  int min_idx = std::numeric_limits<int>::max();
  for (int cell = 0; cell < n_cells; ++cell) {
    for (int neighbor = 0; neighbor < k; ++neighbor) {
      const int idx = knn_idx(neighbor, cell);
      if (idx != NA_INTEGER && idx < min_idx) {
        min_idx = idx;
      }
    }
  }
  const int idx_offset = (min_idx == 0) ? 1 : 0;

  int max_label = 0;
  for (int i = 0; i < batch_labels.size(); ++i) {
    const int label = batch_labels[i];
    if (label != NA_INTEGER && label > max_label) {
      max_label = label;
    }
  }

  NumericVector simpson(n_cells, NumericVector::get_na());
  std::vector<double> label_sums(static_cast<std::size_t>(max_label + 1), 0.0);
  std::vector<int> touched;
  touched.reserve(max_label > 0 ? max_label : 1);

  for (int cell = 0; cell < n_cells; ++cell) {
    std::vector<double> probs = find_neighbor_probs_cpp(D, cell, perplexity, tol, max_iter);

    bool has_positive_prob = false;
    for (double prob : probs) {
      if (prob > 0.0) {
        has_positive_prob = true;
        break;
      }
    }
    if (!has_positive_prob) {
      continue;
    }

    touched.clear();
    for (int neighbor = 0; neighbor < k; ++neighbor) {
      const double prob = probs[neighbor];
      if (prob <= 0.0 || NumericVector::is_na(prob)) {
        continue;
      }

      int idx = knn_idx(neighbor, cell);
      if (idx == NA_INTEGER) {
        continue;
      }
      idx += idx_offset;
      if (idx < 1 || idx > batch_labels.size()) {
        continue;
      }

      const int label = batch_labels[idx - 1];
      if (label == NA_INTEGER || label < 1) {
        continue;
      }

      if (label >= static_cast<int>(label_sums.size())) {
        label_sums.resize(static_cast<std::size_t>(label + 1), 0.0);
      }
      if (label_sums[label] == 0.0) {
        touched.push_back(label);
      }
      label_sums[label] += prob;
    }

    if (touched.empty()) {
      continue;
    }

    double value = 0.0;
    for (int label : touched) {
      const double sum_prob = label_sums[label];
      value += sum_prob * sum_prob;
      label_sums[label] = 0.0;
    }
    simpson[cell] = value;
  }

  return simpson;
}

// [[Rcpp::export]]
NumericMatrix compute_simpson_index_matrix_cpp(
    const NumericMatrix& D,
    const IntegerMatrix& knn_idx,
    const IntegerMatrix& batch_labels,
    double perplexity = 15,
    double tol = 1e-5,
    int max_iter = 50) {
  const int k = D.nrow();
  const int n_cells = D.ncol();
  const int n_labels = batch_labels.ncol();

  if (knn_idx.nrow() != k || knn_idx.ncol() != n_cells) {
    stop("D and knn_idx must have the same dimensions.");
  }
  if (batch_labels.nrow() != n_cells) {
    stop("nrow(batch_labels) must equal ncol(D).");
  }
  if (!std::isfinite(perplexity) || perplexity <= 0.0) {
    stop("perplexity must be a single positive number.");
  }
  if (!std::isfinite(tol) || tol <= 0.0) {
    stop("tol must be a single positive number.");
  }
  if (max_iter < 1) {
    stop("max_iter must be at least 1.");
  }

  int min_idx = std::numeric_limits<int>::max();
  for (int cell = 0; cell < n_cells; ++cell) {
    for (int neighbor = 0; neighbor < k; ++neighbor) {
      const int idx = knn_idx(neighbor, cell);
      if (idx != NA_INTEGER && idx < min_idx) {
        min_idx = idx;
      }
    }
  }
  const int idx_offset = (min_idx == 0) ? 1 : 0;

  int max_label = 0;
  for (int label_col = 0; label_col < n_labels; ++label_col) {
    for (int cell = 0; cell < n_cells; ++cell) {
      const int label = batch_labels(cell, label_col);
      if (label != NA_INTEGER && label > max_label) {
        max_label = label;
      }
    }
  }

  NumericMatrix simpson(n_cells, n_labels);
  std::fill(simpson.begin(), simpson.end(), NumericVector::get_na());

  std::vector<double> label_sums(static_cast<std::size_t>(max_label + 1), 0.0);
  std::vector<int> touched;
  touched.reserve(max_label > 0 ? max_label : 1);

  for (int cell = 0; cell < n_cells; ++cell) {
    std::vector<double> probs = find_neighbor_probs_cpp(D, cell, perplexity, tol, max_iter);

    bool has_positive_prob = false;
    for (double prob : probs) {
      if (prob > 0.0) {
        has_positive_prob = true;
        break;
      }
    }
    if (!has_positive_prob) {
      continue;
    }

    for (int label_col = 0; label_col < n_labels; ++label_col) {
      touched.clear();
      for (int neighbor = 0; neighbor < k; ++neighbor) {
        const double prob = probs[neighbor];
        if (prob <= 0.0 || NumericVector::is_na(prob)) {
          continue;
        }

        int idx = knn_idx(neighbor, cell);
        if (idx == NA_INTEGER) {
          continue;
        }
        idx += idx_offset;
        if (idx < 1 || idx > n_cells) {
          continue;
        }

        const int label = batch_labels(idx - 1, label_col);
        if (label == NA_INTEGER || label < 1) {
          continue;
        }

        if (label >= static_cast<int>(label_sums.size())) {
          label_sums.resize(static_cast<std::size_t>(label + 1), 0.0);
        }
        if (label_sums[label] == 0.0) {
          touched.push_back(label);
        }
        label_sums[label] += prob;
      }

      if (touched.empty()) {
        continue;
      }

      double value = 0.0;
      for (int label : touched) {
        const double sum_prob = label_sums[label];
        value += sum_prob * sum_prob;
        label_sums[label] = 0.0;
      }
      simpson(cell, label_col) = value;
    }
  }

  return simpson;
}

// [[Rcpp::export]]
NumericMatrix compute_lisi_matrix_cpp(
    const NumericMatrix& X,
    const IntegerMatrix& batch_labels,
    int n_neighbors,
    double perplexity = 30,
    double tol = 1e-5,
    int max_iter = 50) {
  const int n = X.nrow();
  const int p = X.ncol();
  const int n_labels = batch_labels.ncol();

  if (n < 2) {
    stop("X must have at least two rows.");
  }
  if (batch_labels.nrow() != n) {
    stop("nrow(batch_labels) must equal nrow(X).");
  }
  if (n_neighbors < 1 || n_neighbors >= n) {
    stop("n_neighbors must be between 1 and nrow(X) - 1.");
  }
  if (!std::isfinite(perplexity) || perplexity <= 0.0) {
    stop("perplexity must be a single positive number.");
  }
  if (!std::isfinite(tol) || tol <= 0.0) {
    stop("tol must be a single positive number.");
  }
  if (max_iter < 1) {
    stop("max_iter must be at least 1.");
  }

  for (int col = 0; col < p; ++col) {
    for (int row = 0; row < n; ++row) {
      if (!std::isfinite(X(row, col))) {
        stop("X must contain only finite numeric values.");
      }
    }
  }

  int max_label = 0;
  for (int label_col = 0; label_col < n_labels; ++label_col) {
    for (int row = 0; row < n; ++row) {
      const int label = batch_labels(row, label_col);
      if (label == NA_INTEGER || label < 1) {
        stop("batch_labels must contain positive integer labels.");
      }
      if (label > max_label) {
        max_label = label;
      }
    }
  }

  NumericMatrix out(n, n_labels);
  std::fill(out.begin(), out.end(), NA_REAL);

  LisiMatrixWorker worker(
    REAL(X),
    INTEGER(batch_labels),
    REAL(out),
    n,
    p,
    n_neighbors,
    n_labels,
    max_label,
    perplexity,
    tol,
    max_iter
  );

  unsigned int thread_count = std::thread::hardware_concurrency();
  if (thread_count == 0) {
    thread_count = 1;
  }
  thread_count = std::min<unsigned int>(thread_count, static_cast<unsigned int>(n));

  if (thread_count <= 1 || n < 128) {
    worker.run(0, n);
  } else {
    std::vector<std::thread> threads;
    threads.reserve(thread_count);
    const std::size_t block = (static_cast<std::size_t>(n) + thread_count - 1) / thread_count;

    for (unsigned int t = 0; t < thread_count; ++t) {
      const std::size_t begin = static_cast<std::size_t>(t) * block;
      const std::size_t end = std::min<std::size_t>(begin + block, static_cast<std::size_t>(n));
      if (begin >= end) {
        break;
      }
      threads.emplace_back([&worker, begin, end]() {
        worker.run(begin, end);
      });
    }

    for (std::thread& thread : threads) {
      thread.join();
    }
  }

  return out;
}

// [[Rcpp::export]]
List lisi_exact_knn_cpp(const NumericMatrix& X, int k, bool exclude_self = false) {
  const int n = X.nrow();
  const int p = X.ncol();

  if (n < 1) {
    stop("X must have at least one row.");
  }
  const int max_k = exclude_self ? n - 1 : n;
  if (k < 1 || k > max_k) {
    stop("k must be between 1 and the number of available neighbors.");
  }

  for (int col = 0; col < p; ++col) {
    for (int row = 0; row < n; ++row) {
      if (!std::isfinite(X(row, col))) {
        stop("X must contain only finite numeric values.");
      }
    }
  }

  IntegerMatrix nn_idx(n, k);
  NumericMatrix nn_dists(n, k);

  ExactKnnWorker worker(
    REAL(X),
    INTEGER(nn_idx),
    REAL(nn_dists),
    n,
    p,
    k,
    exclude_self
  );

  unsigned int thread_count = std::thread::hardware_concurrency();
  if (thread_count == 0) {
    thread_count = 1;
  }
  thread_count = std::min<unsigned int>(thread_count, static_cast<unsigned int>(n));

  if (thread_count <= 1 || n < 128) {
    worker.run(0, n);
  } else {
    std::vector<std::thread> threads;
    threads.reserve(thread_count);
    const std::size_t block = (static_cast<std::size_t>(n) + thread_count - 1) / thread_count;

    for (unsigned int t = 0; t < thread_count; ++t) {
      const std::size_t begin = static_cast<std::size_t>(t) * block;
      const std::size_t end = std::min<std::size_t>(begin + block, static_cast<std::size_t>(n));
      if (begin >= end) {
        break;
      }
      threads.emplace_back([&worker, begin, end]() {
        worker.run(begin, end);
      });
    }

    for (std::thread& thread : threads) {
      thread.join();
    }
  }

  return List::create(
    Named("nn.idx") = nn_idx,
    Named("nn.dists") = nn_dists
  );
}

// [[Rcpp::export]]
List drop_self_from_knn_cpp(
    const IntegerMatrix& nn_idx,
    const NumericMatrix& nn_dists) {
  const int n = nn_idx.nrow();
  const int k = nn_idx.ncol();

  if (nn_dists.nrow() != n || nn_dists.ncol() != k) {
    stop("nn_idx and nn_dists must have the same dimensions.");
  }
  if (k < 2) {
    stop("Need at least two neighbors to remove self from the KNN graph.");
  }

  IntegerMatrix idx_out(n, k - 1);
  NumericMatrix dist_out(n, k - 1);

  for (int i = 0; i < n; ++i) {
    int drop_pos = -1;
    for (int neighbor = 0; neighbor < k; ++neighbor) {
      if (nn_idx(i, neighbor) == (i + 1)) {
        drop_pos = neighbor;
        break;
      }
    }
    if (drop_pos < 0) {
      drop_pos = 0;
    }

    int out_col = 0;
    for (int neighbor = 0; neighbor < k; ++neighbor) {
      if (neighbor == drop_pos) {
        continue;
      }
      idx_out(i, out_col) = nn_idx(i, neighbor);
      dist_out(i, out_col) = nn_dists(i, neighbor);
      ++out_col;
    }
  }

  return List::create(
    Named("nn.idx") = idx_out,
    Named("nn.dists") = dist_out
  );
}
