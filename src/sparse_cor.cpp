#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <vector>

using namespace Rcpp;

// Convert one dense cross-product block directly to sparse triplets while
// applying Pearson centering, scaling, and output filtering in one pass.
//
// [[Rcpp::export]]
List sparse_pearson_block_cpp(
    const S4& cross_products,
    const NumericVector& x_means,
    const NumericVector& y_means,
    const NumericVector& x_sds,
    const NumericVector& y_sds,
    double n_observations,
    bool allow_neg,
    bool remove_na,
    bool remove_inf,
    double threshold) {
  if (!cross_products.is("dgCMatrix")) {
    stop("Pearson cross-products must be supplied as a dgCMatrix");
  }
  const IntegerVector dimensions = cross_products.slot("Dim");
  const IntegerVector column_pointers = cross_products.slot("p");
  const IntegerVector row_indices = cross_products.slot("i");
  const NumericVector stored_values = cross_products.slot("x");
  const int n_source = dimensions[0];
  const int n_target = dimensions[1];

  if (
      x_means.size() != n_source || x_sds.size() != n_source ||
      y_means.size() != n_target || y_sds.size() != n_target) {
    stop("Pearson block dimensions do not match the supplied moments");
  }
  if (n_observations <= 1.0) {
    stop("Invalid Pearson block controls");
  }

  std::vector<int> output_i;
  std::vector<double> output_x;
  std::vector<int> output_counts(static_cast<std::size_t>(n_target), 0);
  const std::size_t cells = static_cast<std::size_t>(n_source) *
    static_cast<std::size_t>(n_target);
  const std::size_t reserve_size = threshold <= 0.0
    ? cells
    : std::min<std::size_t>(cells, 16384U);
  output_i.reserve(reserve_size);
  output_x.reserve(reserve_size);

  const double covariance_denominator = n_observations - 1.0;
  for (int target = 0; target < n_target; ++target) {
    if ((target & 15) == 0) {
      checkUserInterrupt();
    }
    const double target_mean = y_means[target];
    const double target_sd = y_sds[target];
    int stored_position = column_pointers[target];
    const int stored_end = column_pointers[target + 1];
    const std::size_t entries_before = output_x.size();
    for (int source = 0; source < n_source; ++source) {
      double cross_product = 0.0;
      if (
          stored_position < stored_end &&
          row_indices[stored_position] == source) {
        cross_product = stored_values[stored_position];
        ++stored_position;
      }
      double value = (
        cross_product -
        n_observations * x_means[source] * target_mean
      ) / covariance_denominator;
      value /= x_sds[source] * target_sd;

      bool missing = R_IsNA(value) || R_IsNaN(value);
      if (remove_na && missing) {
        value = 0.0;
        missing = false;
      }
      if (remove_inf && std::isinf(value)) {
        value = 0.0;
      }
      if (!allow_neg && !missing && value < 0.0) {
        value = 0.0;
      }
      if (threshold > 0.0 && !missing && std::abs(value) < threshold) {
        value = 0.0;
      }

      if (missing || value != 0.0) {
        output_i.push_back(source);
        output_x.push_back(value);
      }
    }
    output_counts[static_cast<std::size_t>(target)] = static_cast<int>(
      output_x.size() - entries_before
    );
  }

  return List::create(
    Named("i") = output_i,
    Named("x") = output_x,
    Named("counts") = output_counts
  );
}
