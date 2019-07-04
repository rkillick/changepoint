#include <R.h>
#include <Rmath.h>
#include <Rinternals.h> // RK addition
#include <R_ext/RS.h>	// RK addition
#include <R_ext/Lapack.h> // RK addition
#include <R_ext/BLAS.h> // RK addition
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
//#include "cost_general_functions.c" // commented out as implicitly in the workspace as using the package.


void binseg(char** cost_func, //Descibe the cost function used i.e. norm.mean.cost (change in mean in normal distributed data)  
	    double* sumstat, //array of summary statistics of the time series
	    int* n, //Length of the time series
	    int *m,     /* number of dimensions (regressors+1) */
	    double* pen, //Penalty used to decide if a changepoint is significant 
	    int* Q, //Max number of changepoints
	    int* cptsout, //Q length vector of identified changepoint locations 
	    int *error,   /* 0 by default, nonzero indicates error in code */
	    int *minorder, /* minimum order for mll_ar */
	    int *optimalorder, /* vector of optimal orders per segment */
	    int *maxorder, /* maximum order for mll_ar */
	    int* minseglen, //minimum segment length 
	    double* likeout, //Q length vector of likelihood ratio values for changepoints in cptsout 
	    int* op_cps, //Optimal number of changepoint for pen supplied 
	    double* shape, //only used when cost_func is the gamma likelihood 
	    double *tol,    // tolerance only for cpt.reg
	    int *MBIC          // 1 if MBIC penalty, 0 if not
	    )
{  
  double oldmax=1.7E+308,null,lambda[*n],maxout;
  int q,p,i,j,maxid,end;
  for(i=0;i<*n;i++){
    lambda[i]=-INFINITY;
  }
  int tau[*Q+2]; // max ncpts is Q, +2 is for 0 and n
  tau[0]=0;
  tau[1]= *n;
     
    int l = 0; // must use l instead of p
    int np1 = *n + 1; //ncols of summary statistcs array
    int size = (*m * (*m + 1)) * 0.5; //nrows of summary statistics array
    int start;  //indicies
    double cost = 0;   //cost over specified segment

    void (*costfunction)();
    void mean_norm();
    void var_norm();
    void meanvar_norm();
    void meanvar_exp();
    void meanvar_gamma();
    void meanvar_poisson();

    if (strcmp(*cost_func,"var.norm")==0){
    costfunction = &var_norm;
    }
    else if (strcmp(*cost_func,"mean.norm")==0){
    costfunction = &mean_norm;
    }
     else if (strcmp(*cost_func,"meanvar.norm")==0){
   costfunction = &meanvar_norm;
    }
    else if (strcmp(*cost_func,"meanvar.exp")==0){
   costfunction = &meanvar_exp;
   }
    else if (strcmp(*cost_func,"meanvar.gamma")==0){
   costfunction = &meanvar_gamma;
   }
    else if (strcmp(*cost_func,"meanvar.poisson")==0){
   costfunction = &meanvar_poisson;
   }
    else if (strcmp(*cost_func,"mean.norm.mbic")==0){
   costfunction = &mean_norm;
   }
  else if (strcmp(*cost_func,"var.norm.mbic")==0){
   costfunction = &var_norm;
   }
  else if (strcmp(*cost_func,"meanvar.norm.mbic")==0){
   costfunction = &meanvar_norm;
 }
  else if (strcmp(*cost_func,"meanvar.exp.mbic")==0){
   costfunction = &meanvar_exp;
 }
  else if (strcmp(*cost_func,"meanvar.gamma.mbic")==0){
   costfunction = &meanvar_gamma;
 }
  else if (strcmp(*cost_func,"meanvar.poisson.mbic")==0){
   costfunction = &meanvar_poisson;
 }

  void max_which(double*, int, double*, int*);
  void order_vec(int[], int);

    for(q=0;q<*Q;q++){
      R_CheckUserInterrupt(); // checks if user has interrupted the R session and quits if true
       for(p=0;p<*n;p++){lambda[p]=0;}
        i=1;
        start=tau[0];
        end=tau[1];
        costfunction(sumstat, &size, &np1, &l, minorder, optimalorder, maxorder, &start, &end, &cost, tol, error, shape, MBIC);
        null = (-0.5) * cost;

        for(j=2;j<(*n-2);j++){
           if(j==end){
            start=end;
    				i=i+1;
    				end=tau[i];
            costfunction(sumstat, &size, &np1, &l, minorder, optimalorder, maxorder, &start, &end, &cost, tol, error, shape, MBIC);
            null = (-0.5) * cost;
          }else{
    				 if(((j-start-1)>=*minseglen)&&((end-j)>=*minseglen)){
                        double cost1 = 0;
                        double cost2 = 0;
                        costfunction(sumstat, &size, &np1, &p, minorder, optimalorder, maxorder, &start, &j, &cost1, tol, error, shape, MBIC);
                        costfunction(sumstat, &size, &np1, &p, minorder, optimalorder, maxorder, &j, &end, &cost2, tol, error, shape, MBIC);
                        lambda[j] =  ((-0.5) * cost1) + ((-0.5)* cost2) - null;
            }
    			}
        }

    max_which(lambda,*n,&maxval,&maxid);
    *(cptsout+q) = maxid;
		*(likeout+q) = (oldmax<=maxval) ? oldmax : maxval ;
    oldmax= *(likeout+q);
		tau[q+2] = maxid;
    order_vec(tau,q+3);
  }

  int stop=0;
  *op_cps=0;
  while((stop==0)&&(*op_cps < *Q)){
    if((2* *(likeout+*op_cps)) >= *pen){ (*op_cps)++; }
    else{ stop=1; }
  }
}

