#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <cmath>

using namespace Rcpp;
using namespace RcppParallel;

// [[Rcpp::export]]
NumericMatrix asMatrix(NumericVector rp, NumericVector cp, NumericVector z,
                       int nrows, int ncols)
{
  int elementsCount = z.size();

  // Input validation
  if (elementsCount != rp.size() || elementsCount != cp.size())
  {
    stop("The lengths of 'rp', 'cp', and 'z' must be equal.");
  }
  if (nrows <= 0 || ncols <= 0)
  {
    stop("Both 'nrows' and 'ncols' must be positive.");
  }

  NumericMatrix matrix(nrows, ncols);

  // Index validity check and matrix population
  for (int i = 0; i < elementsCount; i++)
  {
    if (rp[i] < 0 || rp[i] >= nrows || cp[i] < 0 || cp[i] >= ncols)
    {
      std::string errMsg = "Index out of bounds at position (" +
                           std::to_string(rp[i]) + ", " +
                           std::to_string(cp[i]) + ").";
      stop(errMsg.c_str());
    }
    matrix(rp[i], cp[i]) = z[i];
  }

  return matrix;
}

// Parallel version reference: https://rcppcore.github.io/RcppParallel/
// [[Rcpp::depends(RcppParallel)]]
struct MatrixFiller : public Worker
{
  const RVector<double> rp;
  const RVector<double> cp;
  const RVector<double> z;
  RMatrix<double> outputMatrix;

  MatrixFiller(const NumericVector rp, const NumericVector cp,
               const NumericVector z, NumericMatrix outputMatrix)
      : rp(rp), cp(cp), z(z), outputMatrix(outputMatrix) {}

  void operator()(std::size_t begin, std::size_t end)
  {
    for (std::size_t i = begin; i < end; ++i)
    {
      outputMatrix(rp[i], cp[i]) = z[i];
    }
  }
};

// [[Rcpp::export]]
NumericMatrix asMatrixParallel(NumericVector rp, NumericVector cp,
                               NumericVector z, int nrows, int ncols)
{
  NumericMatrix outputMatrix(nrows, ncols);

  MatrixFiller matrixFiller(rp, cp, z, outputMatrix);

  int grainSize = std::max(1, static_cast<int>(z.size() / 2000));
  RcppParallel::parallelFor(0, z.size(), matrixFiller, grainSize);

  return outputMatrix;
}
