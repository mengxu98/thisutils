#include <Rcpp.h>
#include <algorithm>

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
  int nrow, ncol;
  CharacterVector matrix_row_names, matrix_col_names;
  NumericMatrix dense_matrix;

  if (Rf_isMatrix(matrix))
  {
    // Dense matrix
    dense_matrix = as<NumericMatrix>(matrix);
    nrow = dense_matrix.nrow();
    ncol = dense_matrix.ncol();
    matrix_row_names = rownames(dense_matrix);
    matrix_col_names = colnames(dense_matrix);
  }
  else if (Rf_inherits(matrix, "dgCMatrix"))
  {
    // Sparse matrix from Matrix package
    Environment Matrix_env = Environment::namespace_env("Matrix");
    Function as_matrix = Matrix_env["as.matrix"];
    dense_matrix = as<NumericMatrix>(as_matrix(matrix));
    nrow = dense_matrix.nrow();
    ncol = dense_matrix.ncol();
    matrix_row_names = rownames(dense_matrix);
    matrix_col_names = colnames(dense_matrix);
  }
  else
  {
    stop("Input must be a matrix or dgCMatrix");
  }

  if (matrix_row_names.length() == 0 || matrix_col_names.length() == 0)
  {
    stop("Input matrix must have both row and column names");
  }

  CharacterVector row_filter;
  CharacterVector col_filter;
  bool use_row_filter = false;
  bool use_col_filter = false;

  if (row_names.isNotNull())
  {
    row_filter = as<CharacterVector>(row_names);
    use_row_filter = true;
  }
  if (col_names.isNotNull())
  {
    col_filter = as<CharacterVector>(col_names);
    use_col_filter = true;
  }

  // Check if there are any valid matches when filters are applied
  bool has_valid_matches = false;
  for (int i = 0; i < nrow; ++i)
  {
    if (use_row_filter && !std::any_of(row_filter.begin(), row_filter.end(),
                                       [&matrix_row_names, i](const String &row)
                                       {
                                         return std::string(row) == std::string(matrix_row_names[i]);
                                       }))
    {
      continue;
    }

    for (int j = 0; j < ncol; ++j)
    {
      if (use_col_filter && !std::any_of(col_filter.begin(), col_filter.end(),
                                         [&matrix_col_names, j](const String &col)
                                         {
                                           return std::string(col) == std::string(matrix_col_names[j]);
                                         }))
      {
        continue;
      }
      has_valid_matches = true;
      break;
    }
    if (has_valid_matches)
      break;
  }

  if (!has_valid_matches)
  {
    warning("No matching row/column names found with the provided filters. Returning empty table.");
    CharacterVector empty_rows(0);
    CharacterVector empty_cols(0);
    NumericVector empty_values(0);

    DataFrame empty_result = DataFrame::create(
        Rcpp::Named("row") = empty_rows,
        Rcpp::Named("col") = empty_cols,
        Rcpp::Named("value") = empty_values);
    return empty_result;
  }

  // First count elements that pass threshold using absolute values
  int valid_count = 0;
  for (int i = 0; i < nrow; ++i)
  {
    if (use_row_filter && !std::any_of(row_filter.begin(), row_filter.end(),
                                       [&matrix_row_names, i](const String &row)
                                       {
                                         return std::string(row) == std::string(matrix_row_names[i]);
                                       }))
    {
      continue;
    }

    for (int j = 0; j < ncol; ++j)
    {
      if (use_col_filter && !std::any_of(col_filter.begin(), col_filter.end(),
                                         [&matrix_col_names, j](const String &col)
                                         {
                                           return std::string(col) == std::string(matrix_col_names[j]);
                                         }))
      {
        continue;
      }

      double weight = dense_matrix(i, j);
      if ((keep_zero || weight != 0) && std::abs(weight) >= threshold)
      {
        valid_count++;
      }
    }
  }

  // Pre-allocate vectors with exact size needed
  CharacterVector row_names_out(valid_count);
  CharacterVector col_names_out(valid_count);
  NumericVector values(valid_count);
  IntegerVector indices(valid_count);

  // Fill vectors using absolute values for threshold comparison
  int idx = 0;
  for (int i = 0; i < nrow; ++i)
  {
    if (use_row_filter && !std::any_of(row_filter.begin(), row_filter.end(),
                                       [&matrix_row_names, i](const String &row)
                                       {
                                         return std::string(row) == std::string(matrix_row_names[i]);
                                       }))
    {
      continue;
    }

    for (int j = 0; j < ncol; ++j)
    {
      if (use_col_filter && !std::any_of(col_filter.begin(), col_filter.end(),
                                         [&matrix_col_names, j](const String &col)
                                         {
                                           return std::string(col) == std::string(matrix_col_names[j]);
                                         }))
      {
        continue;
      }

      double weight = dense_matrix(i, j);
      if ((keep_zero || weight != 0) && std::abs(weight) >= threshold)
      {
        row_names_out[idx] = matrix_row_names[i];
        col_names_out[idx] = matrix_col_names[j];
        values[idx] = weight;
        indices[idx] = idx;
        idx++;
      }
    }
  }

  std::sort(indices.begin(), indices.end(), [&values](int i1, int i2)
            { return std::abs(values[i1]) > std::abs(values[i2]); });

  CharacterVector sorted_rows(valid_count);
  CharacterVector sorted_cols(valid_count);
  NumericVector sorted_values(valid_count);

  for (int i = 0; i < valid_count; i++)
  {
    sorted_rows[i] = row_names_out[indices[i]];
    sorted_cols[i] = col_names_out[indices[i]];
    sorted_values[i] = values[indices[i]];
  }

  DataFrame result =
      DataFrame::create(Rcpp::Named("row") = sorted_rows,
                        Rcpp::Named("col") = sorted_cols,
                        Rcpp::Named("value") = sorted_values);

  return result;
}
