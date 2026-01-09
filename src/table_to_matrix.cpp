#include <Rcpp.h>
#include <algorithm>
#include <unordered_map>

using namespace Rcpp;

//' @title Switch table to matrix
//'
//' @md
//' @param table A table with three columns: `row`, `col`, and `value`.
//' @param row_names Character vector of row names to filter by.
//' @param col_names Character vector of column names to filter by.
//' @param threshold The threshold for filtering values based on absolute values.
//' Defaults to `0`.
//' @param return_sparse Whether to return a sparse matrix. Defaults to `false`.
//'
//' @return A matrix.
//' @export
//' @seealso [matrix_to_table]
//'
//' @examples
//' table <- data.frame(
//'   row = c("r1", "r2", "r3", "r4", "r5", "r6"),
//'   col = c("c4", "c5", "c6", "c1", "c2", "c3"),
//'   value = c(0.6, -0.5, -0.4, 0.3, 0.2, 0.1)
//' )
//' matrix <- table_to_matrix(table)
//' table_new <- matrix_to_table(matrix)
//' identical(table, table_new)
//'
//' table_to_matrix(table, threshold = 0.3)
//'
//' table_to_matrix(
//'   table,
//'   row_names = c("r1", "r2"),
//'   col_names = c("c4", "c5")
//' )
//'
//' sparse_matrix <- simulate_sparse_matrix(10, 10)
//' table_sparse <- matrix_to_table(
//'   sparse_matrix
//' )
//' sparse_matrix_new <- table_to_matrix(
//'   table_sparse,
//'   return_sparse = TRUE
//' )
//' identical(sparse_matrix, sparse_matrix_new)
// [[Rcpp::export]]
SEXP table_to_matrix(DataFrame table,
                     Nullable<CharacterVector> row_names = R_NilValue,
                     Nullable<CharacterVector> col_names = R_NilValue,
                     double threshold = 0.0,
                     bool return_sparse = false)
{
  if (table.size() != 3)
  {
    stop("Input table must have exactly 3 columns");
  }

  CharacterVector table_rows = table[0]; // First column as row names
  CharacterVector table_cols = table[1]; // Second column as column names
  NumericVector values = table[2];       // Third column as values

  if (!Rf_isNumeric(table[2]))
  {
    stop("The third column must be numeric (values)");
  }

  CharacterVector filter_rows;
  CharacterVector filter_cols;

  if (row_names.isNotNull())
  {
    CharacterVector row_names_filter(row_names);
    filter_rows = intersect(unique(table_rows), row_names_filter);
  }
  else
  {
    filter_rows = unique(table_rows);
  }

  if (col_names.isNotNull())
  {
    CharacterVector col_names_filter(col_names);
    filter_cols = intersect(unique(table_cols), col_names_filter);
  }
  else
  {
    filter_cols = unique(table_cols);
  }

  std::vector<std::string> row_strings;
  std::vector<std::string> col_strings;

  for (R_xlen_t i = 0; i < filter_rows.length(); i++)
  {
    row_strings.push_back(Rcpp::as<std::string>(filter_rows[i]));
  }
  for (R_xlen_t i = 0; i < filter_cols.length(); i++)
  {
    col_strings.push_back(Rcpp::as<std::string>(filter_cols[i]));
  }

  auto nameCompare = [](const std::string &a, const std::string &b)
  {
    size_t na = a.find_first_of("0123456789");
    size_t nb = b.find_first_of("0123456789");

    if (na != std::string::npos && nb != std::string::npos)
    {
      std::string prefix_a = a.substr(0, na);
      std::string prefix_b = b.substr(0, nb);
      if (prefix_a == prefix_b)
      {
        return std::stoi(a.substr(na)) < std::stoi(b.substr(nb));
      }
    }
    return a < b;
  };

  std::sort(row_strings.begin(), row_strings.end(), nameCompare);
  std::sort(col_strings.begin(), col_strings.end(), nameCompare);

  std::unordered_map<std::string, int> row_indices;
  std::unordered_map<std::string, int> col_indices;

  for (size_t i = 0; i < row_strings.size(); ++i)
  {
    row_indices[row_strings[i]] = i;
  }
  for (size_t i = 0; i < col_strings.size(); ++i)
  {
    col_indices[col_strings[i]] = i;
  }

  CharacterVector sorted_rows(row_strings.size());
  CharacterVector sorted_cols(col_strings.size());
  for (size_t i = 0; i < row_strings.size(); i++)
  {
    sorted_rows[i] = row_strings[i];
  }
  for (size_t i = 0; i < col_strings.size(); i++)
  {
    sorted_cols[i] = col_strings[i];
  }

  if (return_sparse)
  {
    Environment Matrix_env = Environment::namespace_env("Matrix");
    Function sparseMatrix = Matrix_env["sparseMatrix"];

    std::vector<int> sparse_i, sparse_j;
    std::vector<double> sparse_x;

    for (R_xlen_t i = 0; i < table.nrows(); ++i)
    {
      std::string row_name = Rcpp::as<std::string>(table_rows[i]);
      std::string col_name = Rcpp::as<std::string>(table_cols[i]);

      auto row_it = row_indices.find(row_name);
      auto col_it = col_indices.find(col_name);

      if (row_it != row_indices.end() && col_it != col_indices.end())
      {
        double value = values[i];
        if (std::abs(value) >= threshold && value != 0.0)
        {
          sparse_i.push_back(row_it->second + 1); // R uses 1-based indexing
          sparse_j.push_back(col_it->second + 1);
          sparse_x.push_back(value);
        }
      }
    }

    IntegerVector sparse_i_vec(sparse_i.begin(), sparse_i.end());
    IntegerVector sparse_j_vec(sparse_j.begin(), sparse_j.end());
    NumericVector sparse_x_vec(sparse_x.begin(), sparse_x.end());

    SEXP dims = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(dims)
    [0] = row_strings.size();
    INTEGER(dims)
    [1] = col_strings.size();

    SEXP dimnames = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(dimnames, 0, static_cast<SEXP>(sorted_rows));
    SET_VECTOR_ELT(dimnames, 1, static_cast<SEXP>(sorted_cols));

    SEXP result = sparseMatrix(
        Named("i") = sparse_i_vec,
        Named("j") = sparse_j_vec,
        Named("x") = sparse_x_vec,
        Named("dims") = dims,
        Named("dimnames") = dimnames);

    UNPROTECT(2);
    return result;
  }
  else
  {
    NumericMatrix matrix(row_strings.size(), col_strings.size());
    std::fill(matrix.begin(), matrix.end(), 0.0);

    SEXP dimnames = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(dimnames, 0, sorted_rows);
    SET_VECTOR_ELT(dimnames, 1, sorted_cols);
    Rf_setAttrib(matrix, R_DimNamesSymbol, dimnames);
    UNPROTECT(1);

    for (R_xlen_t i = 0; i < table.nrows(); ++i)
    {
      std::string row_name = Rcpp::as<std::string>(table_rows[i]);
      std::string col_name = Rcpp::as<std::string>(table_cols[i]);

      auto row_it = row_indices.find(row_name);
      auto col_it = col_indices.find(col_name);

      if (row_it != row_indices.end() && col_it != col_indices.end())
      {
        double value = values[i];
        if (std::abs(value) >= threshold)
        {
          matrix(row_it->second, col_it->second) = value;
        }
      }
    }

    return matrix;
  }
}
