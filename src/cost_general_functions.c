#include <R.h> 
#include <Rmath.h>
#include <Rinternals.h> // RK addition
#include <R_ext/RS.h>  // RK addition
#include <R_ext/Lapack.h> // RK addition
#include <R_ext/BLAS.h> // RK addition
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#define SWAP(a,b)   { int t; t=a; a=b; b=t; }  // Macro for swapping

// Cost functions  

double mll_mean(double x, double x2, double x3, int n, double shape){
  return(x2-(x*x)/n);
}

double mll_var(double x, double x2, double x3, int n, double shape){
  if(x3<=0){x3=0.00000000001;}
  return(n*(log(2*M_PI)+log(x3/n)+1)); /* M_PI is in Rmath.h  */
} 

double mll_meanvar(double x, double x2, double x3, int n, double shape){
  double sigsq=(x2-((x*x)/n))/n;
  if(sigsq<=0){sigsq=0.00000000001;}
  return(n*(log(2*M_PI)+log(sigsq)+1)); /* M_PI is in Rmath.h  */
}


double mll_meanvar_exp(double x, double x2, double x3, int n, double shape){
  return(2*n*(log(x)-log(n)));
}

double mll_meanvar_gamma(double x, double x2, double x3, int n, double shape){
  return(2*n*shape*(log(x)-log(n*shape)));
}

double mll_meanvar_poisson(double x, double x2, double x3, int n, double shape){
  if(x==0){return(0);}
  else{return(2*x*(log(n)-log(x)));}
}

double mll_meanvar_binomial(double x, double x2, double x3, int n, double shape){
  //x  = sum of data
  //x2 = sum of size
  if(x==0){return(0);}
  if(x==x2){return(0);}
  else{return(2*x2*log(x2)-2*x*log(x)-2*(x2-x)*log(x2-x));}
}

double mbic_var(double x, double x2, double x3, int n, double shape){
  if(x3<=0){x3=0.00000000001;}
  return(n*(log(2*M_PI)+log(x3/n)+1)+log(n)); /* M_PI is in Rmath.h  */
} 

double mbic_meanvar(double x, double x2, double x3, int n, double shape){
  double sigsq=(x2-((x*x)/n))/n;
  if(sigsq<=0){sigsq=0.00000000001;}
  return(n*(log(2*M_PI)+log(sigsq)+1)+log(n)); /* M_PI is in Rmath.h  */
}


double mbic_mean(double x, double x2, double x3, int n, double shape){
  return(x2-(x*x)/n+log(n));
}

double mbic_meanvar_exp(double x, double x2, double x3, int n, double shape){
  return(2*n*(log(x)-log(n))+log(n));
}

double mbic_meanvar_gamma(double x, double x2, double x3, int n, double shape){
  return(2*n*shape*(log(x)-log(n*shape))+log(n));
}

double mbic_meanvar_poisson(double x, double x2, double x3, int n, double shape){
  if(x==0){return(0);}
  else{return(2*x*(log(n)-log(x))+log(n));}
}

double mbic_meanvar_binomial(double x, double x2, double x3, int n, double shape){
  //x  = sum of data
  //x2 = sum of size
  if(x==0){return(0);}
  if(x==x2){return(0);}
  else{return(2*x2*log(x2)-2*x*log(x)-2*(x2-x)*log(x2-x) + log(n));}
}

void max_which(double *array,int n,double *maxout,int *whichout){
  // Function to find maximum of an array with n elements that is put in max 
  *maxout=*array;
  *whichout=0;
  int i;
  for(i=1;i<n;i++){
    if(*(array+i)> *maxout){
      *maxout= *(array+i);
      *whichout=i;
    }
  }
}

void min_which(double *array,int n,double *minout,int *whichout){
  // Function to find minimum of an array with n elements that is put in min 
  *minout=*array;
  *whichout=0;
  int i;
  for(i=1;i<n;i++){
    if(*(array+i)< *minout){
      *minout= *(array+i); 
      *whichout=i;
    }
  }
}

void order_vec( int a[], int n ){   
  int i, j;
  for(i = 0; i < n; i++){         // Make a pass through the array for each element
                                  for(j = 1; j < (n-i); j++){  		// Go through the array beginning to end
                                                                 if(a[j-1] > a[j])       // If the the first number is greater, swap it 
                                                                 SWAP(a[j-1],a[j]);   
                                  }
  }
}

