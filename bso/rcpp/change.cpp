#include <Rcpp.h>
#include <vector>

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
void change(Rcpp::NumericVector& x) {
  // The C++ function does not return anything (it's void),
  // it only modifies the first element of the vector.
  x.at(0) = 1000.0;
}