#include <R.h>
#include <Rmath.h>
#include <Rinternals.h> // RK addition
#include <R_ext/RS.h>  // RK addition
#include <R_ext/Lapack.h> // RK addition
#include <R_ext/BLAS.h> // RK addition
#include <R_ext/Applic.h>  // ST addition
#include <R_ext/Rdynload.h>  // ST addition
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#define SWAP(a,b)   { int t; t=a; a=b; b=t; }  // Macro for swapping

// Cost functions

void mean_norm(double *SS, int *size, int *n, int *p, int *minorder, int *optimalorder, int *maxorder, int *start, int *end, double *cost, double *tol, int *error, double *shape, int *MBIC){
    double l = *end - *start;
    double x = SS[ *end ]  - SS[ *start ];
    double x2 = SS[ *n + *end ] - SS[ *n + *start ];
    if(*MBIC == 0){
      *cost = x2 - (x*x)/l;
    }else{
      *cost = x2-(x*x)/l+log(l);
    }
}

void var_norm(double *SS, int *size, int *n, int *p, int *minorder, int *optimalorder, int *maxorder, int *start, int *end, double *cost, double *tol, int *error, double *shape, int *MBIC){
    double l = *end - *start;
    double x3 = SS[ *n + *n + *end ] - SS[ *n + *n + *start ];
    if(x3<=0){
        x3=0.00000000001;
    }
    if(*MBIC == 0){
      *cost = l*(log(2*M_PI)+log(x3/l)+1);        /* M_PI is in Rmath.h  */
    }else{
      *cost = l*(log(2*M_PI)+log(x3/l)+1)+log(l); /* M_PI is in Rmath.h  */
    }
}

void meanvar_norm(double *SS, int *size, int *n, int *p, int *minorder, int *optimalorder, int *maxorder, int *start, int *end, double *cost, double *tol, int *error, double *shape, int *MBIC){
    double l = *end - *start;
    double x = SS[ *end ]  - SS[ *start ];
    double x2 = SS[ *n + *end ] - SS[ *n + *start ];
    double sigsq=(x2-((x*x)/l))/l;
    if(sigsq<=0){
      sigsq=0.00000000001;
    }
    if(*MBIC == 0){
      *cost = l*(log(2*M_PI)+log(sigsq)+1); /* M_PI is in Rmath.h */
    }else{
      *cost = l*(log(2*M_PI)+log(sigsq)+1)+log(l); /* M_PI is in Rmath.h  */
    }
}

void meanvar_exp(double *SS, int *size, int *n, int *p, int *minorder, int *optimalorder, int *maxorder, int *start, int *end, double *cost, double *tol, int *error, double *shape, int *MBIC){
    double l = *end - *start;
    double x = SS[ *end ]  - SS[ *start ];
    if(*MBIC == 0){
      *cost = 2*l*(log(x)-log(l));
    }else{
      *cost = 2*l*(log(x)-log(l))+log(l);
    }
}

void meanvar_gamma(double *SS, int *size, int *n, int *p, int *minorder, int *optimalorder, int *maxorder, int *start, int *end, double *cost, double *tol, int *error, double *shape, int *MBIC){
    double l = *end - *start;
    double x = SS[ *end ]  - SS[ *start ];
    if(*MBIC == 0){
      *cost = 2*l*(*shape)*(log(x)-log(l*(*shape)));
    }else{
      *cost = 2*l*(*shape)*(log(x)-log(l*(*shape)))+log(l);
    }
}

void meanvar_poisson(double *SS, int *size, int *n, int *p, int *minorder, int *optimalorder, int *maxorder, int *start, int *end, double *cost, double *tol, int *error, double *shape, int *MBIC){
    double l = *end - *start;
    double x = SS[ *end ]  - SS[ *start ];
    if(x==0){
        *cost = 0;
    }else{
      if(*MBIC == 0){
        *cost = 2*x*(log(l)-log(x));
      }else{
        *cost = 2*x*(log(l)-log(x))+log(l);
      }
    }
}

//Evaluate the regression quadratic cost function based on summary statistics
void mll_reg(double *SS, int *size, int *n, int *p, int *minorder, int *optimalorder, int *maxorder, int *start, int *end, double *cost, double *tol, int *error, double *shape, int *MBIC){
  //SS    - Summary statistics
  //m     - number of rows of SS
  //n     - number of columns of SS
  //p     - number of regressors, nb m = (p+1)*(p+2)/2
  //start - index at start of segment
  //end   - index at end of segment
  //tol   - tolerence for lm
  //error - error index
  //shape - if>0, cost=-2loglike @ fixed shape/variance
  //        if=0, cost=-2logLik()
  //        if<0, cost=RSS
  //MBIC  - 1 if MBIC penalty used, 0 if not.

  //-- memory allocation --
  //Summary statistics for segment
  double *Sumstats = calloc(*size, sizeof(double));
  if(Sumstats == NULL){
    *error = 101;
    goto err101;
  }
  //t(X) %*% X matrix (populated by Sumstats)
  double *XX = calloc(*p * *p, sizeof(double));
  if(XX == NULL){
    *error = 102;
    goto err102;
  }
  //t(X) %*% y vector (populated by Sumstats)
  double *Xy = calloc(*p, sizeof(double));
  if(Xy == NULL){
    *error = 103;
    goto err103;
  }
  //coefficients reordered according to pivot (see below)
  double *beta = calloc(*p, sizeof(double));
  if(beta == NULL){
    *error = 104;
    goto err104;
  }
  //copy of XX (converst to qr on return of dqrls)
  double *qr = calloc(*p * *p, sizeof(double));
  if(qr == NULL){
    *error = 105;
    goto err105;
  }
  //returns coefficient estimates of lm
  double *coef = calloc(*p, sizeof(double));
  if(coef == NULL){
    *error = 106;
    goto err106;
  }
  //returns auxilary information on qr decomposition
  double *qraux = calloc(*p, sizeof(double));
  if(qraux == NULL){
    *error = 107;
    goto err107;
  }
  //working space
  double *work = calloc(2 * *p, sizeof(double));
  if(work == NULL){
    *error = 108;
    goto err108;
  }
  //1:p (changes if cols of XX are permuted)
  int *pivot = calloc(*p, sizeof(int));
  if(pivot == NULL){
    *error = 109;
    goto err109;
  }
  //copy of Xy (converts to y-Xb)
  double *residuals = calloc(*p, sizeof(double));
  if(residuals == NULL){
    *error = 110;
    goto err110;
  }
  //copy of Xy (converts to orthog effects: t(q) %*% Xy)
  double *effects = calloc(*p, sizeof(double));
  if(effects == NULL){
    *error = 111;
    goto err111;
  }
  //copy of Xy
  double *Xytmp = calloc(*p, sizeof(double));
  if(Xytmp == NULL){
    *error = 112;
    goto err112;
  }

  int i, j, tri;           //indices
  int rank = 0;            //returns estimated rank of system (XX)
  int ny = 1;              //number of columns of Xy (here, always 1)
  int nn = *end - *start;  //number of observations
  double RSS;              //residual sum of squares

  //Evaluate summary statistics for segment
  for(i = 0; i < *size; i++){
    //[yy, Xy, XX(lower.tri)]
    Sumstats[i] = SS[*end * *size + i] - SS[*start * *size + i];
  }

  //Populate XX and Xy with values from Sumstats.
  //Also, make copies and initialise values for dqrls
  tri = 0;
  for(i = 0; i < *p; i++){
    Xy[i] = Sumstats[i+1];
    Xytmp[i] = Xy[i];           //copy of Xy
    coef[i] = 0.0;           //initialise to zero
    residuals[i] = Xy[i];    //copy of Xy
    effects[i] = Xy[i];      //copy of Xy
    pivot[i] = i+1;          //index from 1:p
    qraux[i] = 0.0;          //initialise to zero
    work[i] = 0.0;           //initialise first half to zero
    work[i + *p] = 0.0;      //initialise second half to zero
    for(j = i; j < *p; j++){
      XX[i * *p + j] = Sumstats[*p + 1 + tri + j];
      qr[i * *p + j] = XX[i * *p + j];   //copy of XX
    }
    tri += *p - i - 1;
    for(j = 0; j < i; j++){
      XX[i * *p + j] = XX[j * *p + i];   //Copy to make XX symmetric
      qr[i * *p + j] = XX[i * *p + j];   //Copy of XX
    }
  }

  //Call to routine used by lm to solve system of linear equations
  F77_CALL(dqrls)(qr, p, p, Xytmp, &ny, tol, coef, residuals, effects, &rank ,
    pivot, qraux, work);

  //Extract coefficients in order
  for(i = 0; i < *p; i++){
    beta[pivot[i]-1] = coef[i];
  }

  //Calculate the quadratic cost
  RSS = Sumstats[0];
  for(i = 0; i < *p; i++){
    RSS -= 2 * beta[i] * Xy[i];
    for(j = 0; j < *p; j++){
      RSS += beta[i] * beta[j] * XX[i * *p + j];
    }
  }

  if(*shape == 0){
    *cost = nn + nn*log(2 * M_PI * RSS) - nn*log(nn); //-2*logLik()
  }else if(*shape > 0){
    *cost = nn*log(2 * M_PI * *shape) + (RSS / *shape); //-2LL(sig2=shape)
  }else{
    *cost = RSS; //RSS
  }
  if(*MBIC==1){
    //*cost = RSS+log(nn); // extra log(length segment) for MBIC cost
  }
  //Free allocated memory
  free(Xytmp);
  err112: free(effects);
  err111: free(residuals);
  err110: free(pivot);
  err109: free(work);
  err108: free(qraux);
  err107: free(coef);
  err106: free(qr);
  err105: free(beta);
  err104: free(Xy);
  err103: free(XX);
  err102: free(Sumstats);
  err101: return;

}
//Calculate summary statistics for the regression quadratic cost function.
void RegQuadCost_SS(double *X, int *n, int *nc, double *SS, int *m){
  //X - data as double vector of length n*p
  //n - number or rows of X
  //nc - numnber of columns of X
  //SS - summary statistics vector of length (n+1)*m
  //m - number of rows of SS, m = p*(p+1)/2

  int pos;       //position to fill in SS
  int i,j,l;     //loop indicies
  pos = 0;
  //Set first column of SS to zero
  for(i = 0; i < *m; i++){
    SS[pos] = 0;
    pos++;
  }

  //For each new row in X, calculate the unique cross prod of X (lower tri) and
  //   add to previous summary statistic.
  for(i = 0; i < *n; i++){
    for(j = 0; j < *nc; j++){
      for(l = j; l < *nc; l++){
        SS[pos] = SS[pos - *m] + X[j * *n + i]*X[l * *n + i];
        pos++;
      }
    }
  }

  return;
}


//Find the maximum case
void max_which(double *array, int n, double *maxval, int *maxid){
  //array - values for which to find the maximum
  //n - number of items to search
  //maxval - maximum value
  //maxid - index of the maximum value in the array
  int i;
  *maxid = 0;
  *maxval = array[*maxid];
  for(i = 1; i < n; i++){
    if(array[i] > *maxval){
      *maxid = i;
      *maxval = array[i];
    }
  }
  return;
}

//Find the minimum case
void min_which(double *array, int *n, double *minval, int *minid){
  //array - values for which to find the minimum
  //n - number of items to search
  //minval - minimum value
  //minid - index of the minimum value in the array
  int i;
  *minid = 0;
  *minval = array[*minid];
  for(i = 1; i < *n; i++){
    if(array[i] < *minval){
      *minid = i;
      *minval = array[i];
    }
  }
  return;
}



void order_vec( int a[], int n ){
  int i, j;
  for(i = 0; i < n; i++){  // Make a pass through the array for each element
    for(j = 1; j < (n-i); j++){  		// Go through the array beginning to end
      if(a[j-1] > a[j]){       // If the the first number is greater, swap it
        SWAP(a[j-1],a[j]);
      }
    }
  }
}
