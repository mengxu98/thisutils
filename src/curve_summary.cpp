#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>

// [[Rcpp::export]]
Rcpp::List row_ranges(const Rcpp::NumericMatrix& values) {
  const int p = values.nrow(), n = values.ncol();
  Rcpp::NumericVector minimum(p, R_PosInf), maximum(p, R_NegInf);
  const double* data = values.begin();
  for (int j = 0; j < n; ++j) {
    for (int i = 0; i < p; ++i) {
      const double value = *data++;
      if (!R_finite(value)) Rcpp::stop("Spline inputs must be finite.");
      minimum[i] = std::min(minimum[i], value);
      maximum[i] = std::max(maximum[i], value);
    }
  }
  return Rcpp::List::create(Rcpp::_["minimum"] = minimum,
    Rcpp::_["maximum"] = maximum);
}

// [[Rcpp::export]]
Rcpp::List summarize_curves(const Rcpp::NumericMatrix& fitted,
    const Rcpp::NumericMatrix& values, const Rcpp::NumericVector& time,
    bool inclusive) {
  const int p = fitted.nrow(), n = fitted.ncol();
  if (values.nrow() != p || values.ncol() != n || time.size() != n || n == 0)
    Rcpp::stop("Incompatible curve dimensions.");
  for (int j = 0; j < n; ++j)
    if (!R_finite(time[j]) || (j > 0 && time[j] < time[j - 1]))
      Rcpp::stop("Coordinates must be finite and ordered.");
  Rcpp::NumericVector peak(p, NA_REAL), valley(p, NA_REAL);
  Rcpp::IntegerVector above(p);
  std::vector<double> sorted, selected;
  sorted.reserve(n); selected.reserve(n);
  for (int i = 0; i < p; ++i) {
    sorted.clear();
    double minimum = R_PosInf;
    for (int j = 0; j < n; ++j) {
      if (!ISNAN(fitted(i, j))) sorted.push_back(fitted(i, j));
      if (!ISNAN(values(i, j))) minimum = std::min(minimum, values(i, j));
    }
    for (int j = 0; j < n; ++j) above[i] += !ISNAN(values(i, j)) && values(i, j) > minimum;
    if (sorted.empty()) continue;
    for (int side = 0; side < 2; ++side) {
      const double pos = (side == 0 ? 0.99 : 0.01) * (sorted.size() - 1);
      const int low = std::floor(pos), high = std::ceil(pos);
      const double h = pos - low;
      std::nth_element(sorted.begin(), sorted.begin() + low, sorted.end());
      const double lower = sorted[low];
      const double upper = high == low ? lower :
        *std::min_element(sorted.begin() + high, sorted.end());
      const double q = lower == upper ? lower : (1 - h) * lower + h * upper;
      selected.clear();
      bool missing = false;
      for (int j = 0; j < n; ++j) {
        if (ISNAN(fitted(i, j))) { missing = true; continue; }
        const bool keep = side == 0 ? (inclusive ? fitted(i,j) >= q : fitted(i,j) > q) :
          (inclusive ? fitted(i,j) <= q : fitted(i,j) < q);
        if (keep) selected.push_back(time[j]);
      }
      double median = NA_REAL;
      const size_t m = selected.size();
      if (m && !missing) median = m % 2 ? selected[m / 2] :
        (selected[m / 2 - 1] + selected[m / 2]) / 2;
      if (side == 0) peak[i] = median; else valley[i] = median;
    }
  }
  return Rcpp::List::create(Rcpp::_["peak"] = peak, Rcpp::_["valley"] = valley,
    Rcpp::_["n_above_min"] = above);
}
