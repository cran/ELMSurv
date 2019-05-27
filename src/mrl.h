#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

template<typename T>
std::vector<size_t> order(std::vector<T>& values, bool decreasing);

Rcpp::NumericVector  mrl( NumericVector y, NumericVector cen);
