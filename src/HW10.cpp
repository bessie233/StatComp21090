#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix gibbsC(int a, int b,int n, int N =1000) {
  NumericMatrix mat(N, 2);
  double x = 0, y = 0.5;
  mat(0,0) = x; mat(0,1) = y;
  for(int i = 1; i < N; i++) {
    for(int j = 0; j < 2; j++) {
      x = rbinom(1,n,y)[0];
      y = rbeta(1,x+a,n-x+b)[0];
    }
    mat(i, 0) = x;
    mat(i, 1) = y;
  }
  return(mat);
}
