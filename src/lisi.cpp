#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <thread>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace Rcpp;

namespace {

inline bool is_valid_distance(double x) {
  return !NumericVector::is_na(x) && std::isfinite(x);
}

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

inline void maybe_insert_neighbor(std::vector<KnnEntry>& heap, int k, const KnnEntry& candidate) {
  if (static_cast<int>(heap.size()) < k) {
    heap.push_back(candidate);
    std::push_heap(heap.begin(), heap.end(), knn_entry_less);
  } else if (knn_entry_less(candidate, heap.front())) {
    std::pop_heap(heap.begin(), heap.end(), knn_entry_less);
    heap.back() = candidate;
    std::push_heap(heap.begin(), heap.end(), knn_entry_less);
  }
}

inline double row_major_at(const double* x, int row, int col, int p) {
  return x[static_cast<std::size_t>(row) * static_cast<std::size_t>(p) + col];
}

double squared_distance(
    const double* x,
    const double* query,
    int other,
    int p,
    double abandon_above = std::numeric_limits<double>::infinity()) {
  double dist_sq = 0.0;
  for (int col = 0; col < p; ++col) {
    const double diff = query[col] - row_major_at(x, other, col, p);
    dist_sq += diff * diff;
    if (dist_sq > abandon_above) {
      break;
    }
  }
  return dist_sq;
}

std::vector<double> make_row_major_checked(const NumericMatrix& X) {
  const int n = X.nrow();
  const int p = X.ncol();
  std::vector<double> row_major(static_cast<std::size_t>(n) * static_cast<std::size_t>(p), 0.0);

  for (int row = 0; row < n; ++row) {
    for (int col = 0; col < p; ++col) {
      const double value = X(row, col);
      if (!std::isfinite(value)) {
        stop("X must contain only finite numeric values.");
      }
      row_major[static_cast<std::size_t>(row) * static_cast<std::size_t>(p) + col] = value;
    }
  }

  return row_major;
}

struct ClusterIndex {
  const double* x;
  int n;
  int p;
  int n_centers;
  std::vector<double> centers;
  std::vector<int> sizes;
  std::vector<int> offsets;
  std::vector<int> members;
  std::vector<double> member_distances;
};

ClusterIndex build_cluster_index(const double* x, int n, int p) {
  ClusterIndex index;
  index.x = x;
  index.n = n;
  index.p = p;
  index.n_centers = std::max(1, static_cast<int>(std::ceil(std::sqrt(static_cast<double>(n)))));

  const int n_centers = index.n_centers;
  index.centers.resize(static_cast<std::size_t>(n_centers) * static_cast<std::size_t>(p), 0.0);
  for (int center = 0; center < n_centers; ++center) {
    const int source = std::min(n - 1, static_cast<int>((static_cast<double>(center) + 0.5) * n / n_centers));
    std::copy_n(
      x + static_cast<std::size_t>(source) * static_cast<std::size_t>(p),
      p,
      index.centers.begin() + static_cast<std::size_t>(center) * static_cast<std::size_t>(p)
    );
  }

  std::vector<int> assignments(static_cast<std::size_t>(n), 0);
  std::vector<int> counts(static_cast<std::size_t>(n_centers), 0);
  std::vector<double> sums(static_cast<std::size_t>(n_centers) * static_cast<std::size_t>(p), 0.0);

  auto assign_points = [&]() {
    std::fill(counts.begin(), counts.end(), 0);
    std::fill(sums.begin(), sums.end(), 0.0);

    for (int obs = 0; obs < n; ++obs) {
      const double* query = x + static_cast<std::size_t>(obs) * static_cast<std::size_t>(p);
      int best = 0;
      double best_dist = std::numeric_limits<double>::infinity();
      for (int center = 0; center < n_centers; ++center) {
        const double* cptr = index.centers.data() + static_cast<std::size_t>(center) * static_cast<std::size_t>(p);
        double dist_sq = 0.0;
        for (int col = 0; col < p; ++col) {
          const double diff = query[col] - cptr[col];
          dist_sq += diff * diff;
          if (dist_sq > best_dist) {
            break;
          }
        }
        if (dist_sq < best_dist) {
          best_dist = dist_sq;
          best = center;
        }
      }

      assignments[obs] = best;
      ++counts[best];
      double* sum_ptr = sums.data() + static_cast<std::size_t>(best) * static_cast<std::size_t>(p);
      for (int col = 0; col < p; ++col) {
        sum_ptr[col] += query[col];
      }
    }
  };

  for (int iter = 0; iter < 4; ++iter) {
    assign_points();
    for (int center = 0; center < n_centers; ++center) {
      if (counts[center] == 0) {
        continue;
      }
      const double denom = static_cast<double>(counts[center]);
      double* cptr = index.centers.data() + static_cast<std::size_t>(center) * static_cast<std::size_t>(p);
      const double* sum_ptr = sums.data() + static_cast<std::size_t>(center) * static_cast<std::size_t>(p);
      for (int col = 0; col < p; ++col) {
        cptr[col] = sum_ptr[col] / denom;
      }
    }
  }
  assign_points();

  index.sizes = counts;
  index.offsets.resize(static_cast<std::size_t>(n_centers), 0);
  for (int center = 1; center < n_centers; ++center) {
    index.offsets[center] = index.offsets[center - 1] + index.sizes[center - 1];
  }

  std::vector<std::vector<std::pair<double, int> > > by_cluster(static_cast<std::size_t>(n_centers));
  for (int center = 0; center < n_centers; ++center) {
    by_cluster[center].reserve(static_cast<std::size_t>(index.sizes[center]));
  }
  for (int obs = 0; obs < n; ++obs) {
    const int center = assignments[obs];
    const double* query = x + static_cast<std::size_t>(obs) * static_cast<std::size_t>(p);
    const double* cptr = index.centers.data() + static_cast<std::size_t>(center) * static_cast<std::size_t>(p);
    double dist_sq = 0.0;
    for (int col = 0; col < p; ++col) {
      const double diff = query[col] - cptr[col];
      dist_sq += diff * diff;
    }
    by_cluster[center].emplace_back(std::sqrt(std::max(0.0, dist_sq)), obs);
  }

  index.members.resize(static_cast<std::size_t>(n));
  index.member_distances.resize(static_cast<std::size_t>(n));
  for (int center = 0; center < n_centers; ++center) {
    auto& current = by_cluster[center];
    std::sort(current.begin(), current.end());
    const int offset = index.offsets[center];
    for (std::size_t i = 0; i < current.size(); ++i) {
      index.member_distances[static_cast<std::size_t>(offset) + i] = current[i].first;
      index.members[static_cast<std::size_t>(offset) + i] = current[i].second;
    }
  }

  return index;
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
    return x[static_cast<std::size_t>(row) * static_cast<std::size_t>(p) + col];
  }

  void run(std::size_t begin, std::size_t end) const {
    std::vector<KnnEntry> heap;
    heap.reserve(static_cast<std::size_t>(k));
    std::vector<double> query(static_cast<std::size_t>(p), 0.0);

    for (std::size_t i_size = begin; i_size < end; ++i_size) {
      const int i = static_cast<int>(i_size);
      heap.clear();
      for (int col = 0; col < p; ++col) {
        query[col] = at(i, col);
      }

      for (int j = 0; j < n; ++j) {
        if (exclude_self && i == j) {
          continue;
        }

        double dist_sq = 0.0;
        int col = 0;
        for (; col < p; ++col) {
          const double diff = query[col] - at(j, col);
          dist_sq += diff * diff;
          if (static_cast<int>(heap.size()) == k && dist_sq > heap.front().dist_sq) {
            break;
          }
        }
        if (col < p) {
          continue;
        }

        maybe_insert_neighbor(heap, k, KnnEntry{dist_sq, j + 1});
      }

      std::sort_heap(heap.begin(), heap.end(), knn_entry_less);

      for (int neighbor = 0; neighbor < k; ++neighbor) {
        nn_idx[i + n * neighbor] = heap[neighbor].index;
        nn_dists[i + n * neighbor] = std::sqrt(std::max(0.0, heap[neighbor].dist_sq));
      }
    }
  }
};

double hbeta_distances_cpp(
    const std::vector<double>& distances,
    int k,
    double beta,
    std::vector<double>& probs) {
  const float beta_f = static_cast<float>(beta);
  double sum_probs_double = 0.0;
  double weighted_sum_double = 0.0;

  for (int neighbor = 0; neighbor < k; ++neighbor) {
    const double dist = distances[neighbor];
    const double prob = std::exp(-dist * static_cast<double>(beta_f));
    probs[neighbor] = prob;
    sum_probs_double += prob;
    weighted_sum_double += dist * prob;
  }

  const float sum_probs = static_cast<float>(sum_probs_double);
  if (!(sum_probs > 0.0) || !std::isfinite(sum_probs)) {
    std::fill(probs.begin(), probs.end(), 0.0);
    return 0.0;
  }

  for (int neighbor = 0; neighbor < k; ++neighbor) {
    probs[neighbor] /= sum_probs;
  }

  const float entropy = static_cast<float>(
    std::log(sum_probs) + static_cast<double>(beta_f) * weighted_sum_double / sum_probs
  );
  return static_cast<double>(entropy);
}

void find_neighbor_probs_distances_cpp(
    const std::vector<double>& distances,
    int k,
    double perplexity,
    double tol,
    int max_iter,
    std::vector<double>& probs) {
  const float target_entropy = static_cast<float>(std::log(perplexity));
  float beta = 1.0;
  float beta_min = -std::numeric_limits<float>::infinity();
  float beta_max = std::numeric_limits<float>::infinity();

  double entropy = hbeta_distances_cpp(distances, k, beta, probs);
  float entropy_diff = static_cast<float>(entropy) - target_entropy;
  int tries = 0;

  while (std::abs(entropy_diff) > tol && tries < max_iter) {
    if (entropy_diff > 0.0) {
      beta_min = beta;
      beta = std::isfinite(beta_max) ? (beta + beta_max) / 2.0 : beta * 2.0;
    } else {
      beta_max = beta;
      beta = std::isfinite(beta_min) ? (beta + beta_min) / 2.0 : beta / 2.0;
    }

    entropy = hbeta_distances_cpp(distances, k, beta, probs);
    entropy_diff = static_cast<float>(entropy) - target_entropy;
    ++tries;
  }
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
    return x[static_cast<std::size_t>(row) * static_cast<std::size_t>(p) + col];
  }

  inline int label_at(int row, int label_col) const {
    return batch_labels[row + n * label_col];
  }

  void run(std::size_t begin, std::size_t end) const {
    std::vector<KnnEntry> heap;
    heap.reserve(static_cast<std::size_t>(k));
    std::vector<double> distances(static_cast<std::size_t>(k), 0.0);
    std::vector<double> query(static_cast<std::size_t>(p), 0.0);
    std::vector<double> probs(static_cast<std::size_t>(k), 0.0);
    std::vector<float> label_sums(static_cast<std::size_t>(max_label + 1), 0.0);
    std::vector<int> touched;
    touched.reserve(max_label > 0 ? max_label : 1);

    for (std::size_t i_size = begin; i_size < end; ++i_size) {
      const int cell = static_cast<int>(i_size);
      heap.clear();
      for (int col = 0; col < p; ++col) {
        query[col] = at(cell, col);
      }

      for (int other = 0; other < n; ++other) {
        if (cell == other) {
          continue;
        }

        double dist_sq = 0.0;
        int col = 0;
        for (; col < p; ++col) {
          const double diff = query[col] - at(other, col);
          dist_sq += diff * diff;
          if (static_cast<int>(heap.size()) == k && dist_sq > heap.front().dist_sq) {
            break;
          }
        }
        if (col < p) {
          continue;
        }
        maybe_insert_neighbor(heap, k, KnnEntry{dist_sq, other + 1});
      }

      std::sort_heap(heap.begin(), heap.end(), knn_entry_less);
      for (int neighbor = 0; neighbor < k; ++neighbor) {
        distances[neighbor] = std::sqrt(std::max(0.0, heap[neighbor].dist_sq));
      }

      find_neighbor_probs_distances_cpp(
        distances,
        k,
        perplexity,
        tol,
        max_iter,
        probs
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

          const int idx = heap[neighbor].index;
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
          label_sums[label] += static_cast<float>(prob);
        }

        if (touched.empty()) {
          continue;
        }

        float simpson = 0.0;
        for (int label : touched) {
          const float sum_prob = label_sums[label];
          simpson += sum_prob * sum_prob;
          label_sums[label] = 0.0;
        }
        out[cell + n * label_col] = simpson > 0.0 ? 1.0 / static_cast<double>(simpson) : NA_REAL;
      }
    }
  }
};

struct ClusteredLisiMatrixWorker {
  const ClusterIndex& cluster_index;
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

  ClusteredLisiMatrixWorker(
    const ClusterIndex& cluster_index,
    const int* batch_labels,
    double* out,
    int k,
    int n_labels,
    int max_label,
    double perplexity,
    double tol,
    int max_iter
  ) : cluster_index(cluster_index),
      batch_labels(batch_labels),
      out(out),
      n(cluster_index.n),
      p(cluster_index.p),
      k(k),
      n_labels(n_labels),
      max_label(max_label),
      perplexity(perplexity),
      tol(tol),
      max_iter(max_iter) {}

  inline int label_at(int row, int label_col) const {
    return batch_labels[row + n * label_col];
  }

  void search_cell(
      int cell,
      std::vector<KnnEntry>& heap,
      std::vector<std::pair<double, int> >& center_order) const {
    const double* x = cluster_index.x;
    const double* query = x + static_cast<std::size_t>(cell) * static_cast<std::size_t>(p);
    heap.clear();
    center_order.clear();
    center_order.reserve(static_cast<std::size_t>(cluster_index.n_centers));

    for (int center = 0; center < cluster_index.n_centers; ++center) {
      const double* cptr = cluster_index.centers.data() + static_cast<std::size_t>(center) * static_cast<std::size_t>(p);
      double dist_sq = 0.0;
      for (int col = 0; col < p; ++col) {
        const double diff = query[col] - cptr[col];
        dist_sq += diff * diff;
      }
      center_order.emplace_back(std::sqrt(std::max(0.0, dist_sq)), center);
    }
    std::sort(center_order.begin(), center_order.end());

    for (const auto& center_pair : center_order) {
      const double center_dist = center_pair.first;
      const int center = center_pair.second;
      const int size = cluster_index.sizes[center];
      if (size == 0) {
        continue;
      }
      const int offset = cluster_index.offsets[center];
      const double* dptr = cluster_index.member_distances.data() + offset;

      int first = 0;
      double threshold = std::numeric_limits<double>::infinity();
      double threshold_sq = std::numeric_limits<double>::infinity();
      if (static_cast<int>(heap.size()) == k) {
        threshold_sq = heap.front().dist_sq;
        threshold = std::sqrt(std::max(0.0, threshold_sq));
        const double lower = center_dist - threshold;
        if (dptr[size - 1] < lower) {
          continue;
        }
        first = std::lower_bound(dptr, dptr + size, lower) - dptr;
      }

      for (int pos = first; pos < size; ++pos) {
        if (static_cast<int>(heap.size()) == k) {
          threshold_sq = heap.front().dist_sq;
          threshold = std::sqrt(std::max(0.0, threshold_sq));
          if (dptr[pos] > center_dist + threshold) {
            break;
          }
        }

        const int other = cluster_index.members[static_cast<std::size_t>(offset) + pos];
        if (other == cell) {
          continue;
        }

        const double dist_sq = squared_distance(x, query, other, p, threshold_sq);
        if (static_cast<int>(heap.size()) < k || dist_sq <= heap.front().dist_sq) {
          maybe_insert_neighbor(heap, k, KnnEntry{dist_sq, other + 1});
        }
      }
    }

    std::sort_heap(heap.begin(), heap.end(), knn_entry_less);
  }

  void run(std::size_t begin, std::size_t end) const {
    std::vector<KnnEntry> heap;
    heap.reserve(static_cast<std::size_t>(k));
    std::vector<std::pair<double, int> > center_order;
    std::vector<double> distances(static_cast<std::size_t>(k), 0.0);
    std::vector<double> probs(static_cast<std::size_t>(k), 0.0);
    std::vector<float> label_sums(static_cast<std::size_t>(max_label + 1), 0.0);
    std::vector<int> touched;
    touched.reserve(max_label > 0 ? max_label : 1);

    for (std::size_t i_size = begin; i_size < end; ++i_size) {
      const int cell = static_cast<int>(i_size);
      search_cell(cell, heap, center_order);
      for (int neighbor = 0; neighbor < k; ++neighbor) {
        distances[neighbor] = std::sqrt(std::max(0.0, heap[neighbor].dist_sq));
      }

      find_neighbor_probs_distances_cpp(
        distances,
        k,
        perplexity,
        tol,
        max_iter,
        probs
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

          const int idx = heap[neighbor].index;
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
          label_sums[label] += static_cast<float>(prob);
        }

        if (touched.empty()) {
          continue;
        }

        float simpson = 0.0;
        for (int label : touched) {
          const float sum_prob = label_sums[label];
          simpson += sum_prob * sum_prob;
          label_sums[label] = 0.0;
        }
        out[cell + n * label_col] = simpson > 0.0 ? 1.0 / static_cast<double>(simpson) : NA_REAL;
      }
    }
  }
};

template<class Worker_>
void run_parallel_worker(const Worker_& worker, int n) {
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
}

} // namespace

// [[Rcpp::export]]
NumericMatrix compute_lisi_matrix(
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

  std::vector<double> x_row_major = make_row_major_checked(X);

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

  const bool use_clustered_search = n >= 4096 && n_neighbors * 4 < n;
  if (use_clustered_search) {
    ClusterIndex cluster_index = build_cluster_index(x_row_major.data(), n, p);
    ClusteredLisiMatrixWorker worker(
      cluster_index,
      INTEGER(batch_labels),
      REAL(out),
      n_neighbors,
      n_labels,
      max_label,
      perplexity,
      tol,
      max_iter
    );
    run_parallel_worker(worker, n);
  } else {
    LisiMatrixWorker worker(
      x_row_major.data(),
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
    run_parallel_worker(worker, n);
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

  std::vector<double> x_row_major = make_row_major_checked(X);

  IntegerMatrix nn_idx(n, k);
  NumericMatrix nn_dists(n, k);

  ExactKnnWorker worker(
    x_row_major.data(),
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
