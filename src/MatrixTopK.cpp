#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>

using namespace Rcpp;

struct ColumnTopKEntry {
  int row;
  double value;
};

static bool column_topk_missing(double value) {
  return R_IsNA(value) || R_IsNaN(value);
}

static bool column_topk_desc(const ColumnTopKEntry& a, const ColumnTopKEntry& b) {
  const bool a_na = column_topk_missing(a.value);
  const bool b_na = column_topk_missing(b.value);
  if (a_na != b_na) {
    return !a_na;
  }
  if (a.value > b.value) {
    return true;
  }
  if (a.value < b.value) {
    return false;
  }
  return a.row < b.row;
}

static bool column_topk_asc(const ColumnTopKEntry& a, const ColumnTopKEntry& b) {
  const bool a_na = column_topk_missing(a.value);
  const bool b_na = column_topk_missing(b.value);
  if (a_na != b_na) {
    return !a_na;
  }
  if (a.value < b.value) {
    return true;
  }
  if (a.value > b.value) {
    return false;
  }
  return a.row < b.row;
}

// [[Rcpp::export]]
List sparse_topk_by_column(
    S4 mat,
    int k,
    bool decreasing = true,
    bool include_implicit_zeros = true) {
  if (k < 1) {
    stop("k must be positive");
  }

  IntegerVector dims = mat.slot("Dim");
  const int n_rows = dims[0];
  const int n_cols = dims[1];
  IntegerVector row_idx = mat.slot("i");
  IntegerVector col_ptr = mat.slot("p");
  NumericVector values = mat.slot("x");

  IntegerMatrix idx(n_cols, k);
  NumericMatrix top_values(n_cols, k);
  std::fill(idx.begin(), idx.end(), NA_INTEGER);
  std::fill(top_values.begin(), top_values.end(), NA_REAL);

  for (int col = 0; col < n_cols; ++col) {
    const int start = col_ptr[col];
    const int end = col_ptr[col + 1];
    const int n_entries = end - start;
    if (n_entries <= 0 && !include_implicit_zeros) {
      continue;
    }

    std::vector<ColumnTopKEntry> entries;
    entries.reserve(
      static_cast<std::size_t>(n_entries) +
      static_cast<std::size_t>(include_implicit_zeros ? std::min(k, n_rows - n_entries) : 0)
    );
    for (int ptr = start; ptr < end; ++ptr) {
      ColumnTopKEntry entry;
      entry.row = row_idx[ptr] + 1;
      entry.value = values[ptr];
      entries.push_back(entry);
    }

    // A sparse matrix represents every unstored position as zero. Only the
    // first k implicit-zero row indices are needed: no top-k result can contain
    // more than k zeros, and ties are resolved by increasing row index.
    if (include_implicit_zeros) {
      int ptr = start;
      int implicit_added = 0;
      for (int row = 0; row < n_rows && implicit_added < k; ++row) {
        while (ptr < end && row_idx[ptr] < row) {
          ++ptr;
        }
        if (ptr < end && row_idx[ptr] == row) {
          continue;
        }
        entries.push_back(ColumnTopKEntry{row + 1, 0.0});
        ++implicit_added;
      }
    }

    const int n_candidates = static_cast<int>(entries.size());
    const int take = std::min(k, n_candidates);
    if (take < n_candidates) {
      if (decreasing) {
        std::partial_sort(entries.begin(), entries.begin() + take, entries.end(), column_topk_desc);
        std::sort(entries.begin(), entries.begin() + take, column_topk_desc);
      } else {
        std::partial_sort(entries.begin(), entries.begin() + take, entries.end(), column_topk_asc);
        std::sort(entries.begin(), entries.begin() + take, column_topk_asc);
      }
    } else {
      if (decreasing) {
        std::sort(entries.begin(), entries.end(), column_topk_desc);
      } else {
        std::sort(entries.begin(), entries.end(), column_topk_asc);
      }
    }

    for (int rank = 0; rank < take; ++rank) {
      idx(col, rank) = entries[rank].row;
      top_values(col, rank) = entries[rank].value;
    }
  }

  return List::create(
    _["idx"] = idx,
    _["value"] = top_values
  );
}

// [[Rcpp::export]]
List dense_topk_by_column(NumericMatrix mat, int k, bool decreasing = false) {
  if (k < 1) {
    stop("k must be positive");
  }

  const int n_rows = mat.nrow();
  const int n_cols = mat.ncol();
  IntegerMatrix idx(n_cols, k);
  NumericMatrix top_values(n_cols, k);
  std::fill(idx.begin(), idx.end(), NA_INTEGER);
  std::fill(top_values.begin(), top_values.end(), NA_REAL);

  std::vector<ColumnTopKEntry> entries;
  entries.reserve(n_rows);

  for (int col = 0; col < n_cols; ++col) {
    entries.clear();
    for (int row = 0; row < n_rows; ++row) {
      ColumnTopKEntry entry;
      entry.row = row + 1;
      entry.value = mat(row, col);
      entries.push_back(entry);
    }

    const int take = std::min(k, n_rows);
    if (take < n_rows) {
      if (decreasing) {
        std::partial_sort(entries.begin(), entries.begin() + take, entries.end(), column_topk_desc);
        std::sort(entries.begin(), entries.begin() + take, column_topk_desc);
      } else {
        std::partial_sort(entries.begin(), entries.begin() + take, entries.end(), column_topk_asc);
        std::sort(entries.begin(), entries.begin() + take, column_topk_asc);
      }
    } else {
      if (decreasing) {
        std::sort(entries.begin(), entries.end(), column_topk_desc);
      } else {
        std::sort(entries.begin(), entries.end(), column_topk_asc);
      }
    }

    for (int rank = 0; rank < take; ++rank) {
      idx(col, rank) = entries[rank].row;
      top_values(col, rank) = entries[rank].value;
    }
  }

  return List::create(
    _["idx"] = idx,
    _["value"] = top_values
  );
}
