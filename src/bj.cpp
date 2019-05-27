#include "global.h"
#include "mrl.h"
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
Rcpp::NumericVector  bjimpute( SEXP y, SEXP cen,SEXP x, SEXP inibeta) {
	//SEXP logscale, SEXP inibeta, SEXP maxit, SEXP tol, SEXP trace
   int logscale=1;
   int maxit=500;
   double tol=1e-6;
  
  //convert a dataframe to matrix
	Rcpp:: NumericMatrix xNM=testDFtoNM(x);
	int n = xNM.nrow();
	int p = xNM.ncol();
 
  //clone:easy way to convert R vector to Rcpp vectors
    Rcpp:: NumericVector cennew=clone(cen);
   //log transform
	Rcpp:: NumericVector ynew(n);
	if (logscale==1) ynew=log(clone(y));   
	   
	Rcpp:: NumericVector beta(p);	
   	if (inibeta!=R_NilValue)
         beta=clone(inibeta);

	
	
    Rcpp:: NumericMatrix betamat(maxit,beta.size());
	//set the first column of betamat to beta
    betamat( 0, _)=beta; 
    //centering the x values
	NumericMatrix newx=scaleNM(xNM)["newx"];
   
    int iter = 0;
    double dif = 1;
   	
	Rcpp:: NumericVector ystar=clone(ynew);
	Rcpp:: NumericVector eps (ynew.size());
	Rcpp:: NumericVector epstar (ynew.size());	
    Rcpp:: NumericVector tmp (maxit);
	//change abs to fabs
	 while (fabs(dif)>=tol && iter<maxit) {
    
       iter++;
	   arma::vec tmp=mv_mult(xNM,beta);
	   Rcpp:: NumericVector epstemp(tmp.begin(), tmp.end());
       eps = ynew - epstemp;
       epstar = mrl(eps,cennew);
	    // fit model y ~ X
	   arma::colvec coef= arma::solve(as <arma::mat>(newx), as <arma::vec>(ystar));
	   
	   Rcpp:: NumericVector betatemp(coef.begin(), coef.end());
	      
	   //check convergence	
        betamat( iter, _)=betatemp; 
		 for (int j=0; j<iter;j++) {
			 //change abs to fabs
          tmp[iter]=max(abs(betamat(j,_)-betatemp));
        }  
	     	   
	   dif=min(tmp);
	   beta=betatemp;
	   ystar  = epstemp + epstar;
     }
   
   
 
  
  // Rcpp::List ret;
 
  // ret["ystar"] = exp(ystar);
  // ret["beta"] = beta;

 return (exp(ystar));
}
