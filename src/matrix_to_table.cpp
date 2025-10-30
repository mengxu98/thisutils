#include <Rcpp.h>
#include <algorithm>
#include <unordered_set>

using namespace Rcpp;

//' @title Switch matrix to table
//'
//' @md
//' @param matrix A matrix.
//' @param row_names Character vector of row names to filter by.
//' @param col_names Character vector of column names to filter by.
//' @param threshold The threshold for filtering values based on absolute values.
//' Defaults to `0`.
//' @param keep_zero Whether to keep zero values in the table. Defaults to `false`.
//'
//' @return A table with three columns: `row`, `col`, and `value`.
//' @export
//'
//' @examples
//' test_matrix <- simulate_sparse_matrix(10, 10)
//' colnames(test_matrix) <- paste0("c", 1:10)
//' rownames(test_matrix) <- paste0("r", 1:10)
//' table <- matrix_to_table(test_matrix)
//' matrix_new <- table_to_matrix(table)
//' test_matrix <- test_matrix[rownames(matrix_new), colnames(matrix_new)] |>
//'   as_matrix()
//' identical(test_matrix, matrix_new)
//'
//' matrix_to_table(
//'   test_matrix,
//'   threshold = 2
//' )
//'
//' matrix_to_table(
//'   test_matrix,
//'   row_names = c("r1", "r2"),
//'   col_names = c("c1", "c2")
//' )
// [[Rcpp::export]]
DataFrame matrix_to_table(SEXP matrix,
                          Nullable<CharacterVector> row_names = R_NilValue,
                          Nullable<CharacterVector> col_names = R_NilValue,
                          double threshold = 0.0,
                          bool keep_zero = false)
{
  // Handle different matrix types
  int nrow = 0, ncol = 0;
  CharacterVector matrix_row_names, matrix_col_names;

  // Filters as unordered_set for O(1) lookup
  std::unordered_set<std::string> row_filter_set;
  std::unordered_set<std::string> col_filter_set;
  bool use_row_filter = false;
  bool use_col_filter = false;
  if (row_names.isNotNull())
  {
    CharacterVector rf = as<CharacterVector>(row_names);
    for (auto it = rf.begin(); it != rf.end(); ++it)
    {
      row_filter_set.insert(std::string(as<std::string>(*it)));
    }
    use_row_filter = rf.size() > 0;
  }
  if (col_names.isNotNull())
  {
    CharacterVector cf = as<CharacterVector>(col_names);
    for (auto it = cf.begin(); it != cf.end(); ++it)
    {
      col_filter_set.insert(std::string(as<std::string>(*it)));
    }
    use_col_filter = cf.size() > 0;
  }

  std::vector<std::string> out_rows;
  std::vector<std::string> out_cols;
  std::vector<double> out_vals;

  if (Rf_isMatrix(matrix))
  {
    // Dense matrix path
    NumericMatrix dense_matrix = as<NumericMatrix>(matrix);
    nrow = dense_matrix.nrow();
    ncol = dense_matrix.ncol();
    matrix_row_names = rownames(dense_matrix);
    matrix_col_names = colnames(dense_matrix);

    if (matrix_row_names.length() == 0 || matrix_col_names.length() == 0)
    {
      stop("Input matrix must have both row and column names");
    }

    out_rows.reserve(static_cast<size_t>(nrow) * 4);
    out_cols.reserve(static_cast<size_t>(ncol) * 4);
    out_vals.reserve(static_cast<size_t>(nrow) * 4);

    for (int i = 0; i < nrow; ++i)
    {
      std::string rname = as<std::string>(matrix_row_names[i]);
      if (use_row_filter && row_filter_set.find(rname) == row_filter_set.end())
      {
        continue;
      }
      for (int j = 0; j < ncol; ++j)
      {
        std::string cname = as<std::string>(matrix_col_names[j]);
        if (use_col_filter && col_filter_set.find(cname) == col_filter_set.end())
        {
          continue;
        }
        double v = dense_matrix(i, j);
        if (std::abs(v) >= threshold && (keep_zero || v != 0.0))
        {
          out_rows.push_back(rname);
          out_cols.push_back(cname);
          out_vals.push_back(v);
        }
      }
    }
  }
  else if (Rf_inherits(matrix, "dgCMatrix"))
  {
    // Sparse dgCMatrix path without densifying
    S4 spmat(matrix);
    IntegerVector Dim = spmat.slot("Dim");
    nrow = Dim[0];
    ncol = Dim[1];

    List Dimnames = spmat.slot("Dimnames");
    CharacterVector rnames = Dimnames[0];
    CharacterVector cnames = Dimnames[1];
    if (rnames.size() == 0 || cnames.size() == 0)
    {
      stop("Input matrix must have both row and column names");
    }
    matrix_row_names = rnames;
    matrix_col_names = cnames;

    IntegerVector p = spmat.slot("p"); // column pointers (size ncol+1)
    IntegerVector i = spmat.slot("i"); // row indices (0-based)
    NumericVector x = spmat.slot("x"); // non-zero values

    // Reserve approximate capacity
    out_rows.reserve(x.size());
    out_cols.reserve(x.size());
    out_vals.reserve(x.size());

    for (int col = 0; col < ncol; ++col)
    {
      std::string cname = as<std::string>(matrix_col_names[col]);
      if (use_col_filter && col_filter_set.find(cname) == col_filter_set.end())
      {
        continue;
      }
      int start = p[col];
      int end = p[col + 1];
      for (int idx = start; idx < end; ++idx)
      {
        int row = i[idx]; // 0-based
        std::string rname = as<std::string>(matrix_row_names[row]);
        if (use_row_filter && row_filter_set.find(rname) == row_filter_set.end())
        {
          continue;
        }
        double v = x[idx];
        if (std::abs(v) >= threshold && (keep_zero || v != 0.0))
        {
          out_rows.push_back(rname);
          out_cols.push_back(cname);
          out_vals.push_back(v);
        }
      }
    }
  }
  else
  {
    stop("Input must be a matrix or dgCMatrix");
  }

  // If no matches after filtering
  if (out_rows.empty())
  {
    CharacterVector empty_rows(0);
    CharacterVector empty_cols(0);
    NumericVector empty_values(0);
    DataFrame empty_result = DataFrame::create(
        Rcpp::Named("row") = empty_rows,
        Rcpp::Named("col") = empty_cols,
        Rcpp::Named("value") = empty_values);
    return empty_result;
  }

  // Indices for sorting by |value| desc
  IntegerVector order_idx(out_vals.size());
  for (R_xlen_t k = 0; k < order_idx.size(); ++k)
    order_idx[k] = k;
  std::sort(order_idx.begin(), order_idx.end(), [&out_vals](int a, int b)
            { return std::abs(out_vals[a]) > std::abs(out_vals[b]); });

  CharacterVector sorted_rows(order_idx.size());
  CharacterVector sorted_cols(order_idx.size());
  NumericVector sorted_values(order_idx.size());
  for (R_xlen_t k = 0; k < order_idx.size(); ++k)
  {
    int idx = order_idx[k];
    sorted_rows[k] = out_rows[idx];
    sorted_cols[k] = out_cols[idx];
    sorted_values[k] = out_vals[idx];
  }

  DataFrame result = DataFrame::create(
      Rcpp::Named("row") = sorted_rows,
      Rcpp::Named("col") = sorted_cols,
      Rcpp::Named("value") = sorted_values);

  return result;
}
