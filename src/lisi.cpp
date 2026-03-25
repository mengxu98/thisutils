#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>
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
List lisi_exact_knn_cpp(const NumericMatrix& X, int k) {
  const int n = X.nrow();
  const int p = X.ncol();

  if (n < 1) {
    stop("X must have at least one row.");
  }
  if (k < 1 || k > n) {
    stop("k must be between 1 and nrow(X).");
  }

  IntegerMatrix nn_idx(n, k);
  NumericMatrix nn_dists(n, k);
  std::vector< std::pair<double, int> > dist_idx(static_cast<std::size_t>(n));

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      double dist_sq = 0.0;
      for (int col = 0; col < p; ++col) {
        const double xi = X(i, col);
        const double xj = X(j, col);
        if (!std::isfinite(xi) || !std::isfinite(xj)) {
          stop("X must contain only finite numeric values.");
        }
        const double diff = xi - xj;
        dist_sq += diff * diff;
      }
      dist_idx[j] = std::make_pair(dist_sq, j + 1);
    }

    std::partial_sort(
      dist_idx.begin(),
      dist_idx.begin() + k,
      dist_idx.end(),
      [](const std::pair<double, int>& a, const std::pair<double, int>& b) {
        if (a.first == b.first) {
          return a.second < b.second;
        }
        return a.first < b.first;
      }
    );

    for (int neighbor = 0; neighbor < k; ++neighbor) {
      nn_idx(i, neighbor) = dist_idx[neighbor].second;
      nn_dists(i, neighbor) = std::sqrt(std::max(0.0, dist_idx[neighbor].first));
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
