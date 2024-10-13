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

static int *checklist;
static double *tmplike;
static double *Sumstats;


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

//Evaluate the regression quadratic cost function based on summary statistics
void RegQuadCostFunc(double *SS, int *m, int *n, int *p, int *start, int *end,
  double *cost, double *tol, int *error, double *scale, int *MBIC){
  //SS    - Summary statistics
  //m     - number of rows of SS
  //n     - number of columns of SS
  //p     - number of regressors, nb m = (p+1)*(p+2)/2
  //start - index at start of segment
  //end   - index at end of segment
  //tol   - tolerence for lm
  //error - error index
  //scale - if>0, cost=-2loglike @ fixed scale/variance
  //        if=0, cost=-2logLik()
  //        if<0, cost=RSS
  //MBIC  - 1 if MBIC penalty used, 0 if not.

  //-- memory allocation --
  //Summary statistics for segment
  double *Sumstats = calloc(*m, sizeof(double));
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
  for(i = 0; i < *m; i++){
    //[yy, Xy, XX(lower.tri)]
    Sumstats[i] = SS[*end * *m + i] - SS[*start * *m + i];
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

  if(*scale == 0){
    *cost = nn + nn*log(2 * M_PI * RSS) - nn*log(nn); //-2*logLik()
  }else if(*scale > 0){
    *cost = nn*log(2 * M_PI * *scale) + (RSS / *scale); //-2LL(sig2=scale)
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


//Determine cpts for Normal regression under method PELT
void CptReg_Normal_PELT(double *data, int *n, int *m, double *pen, int *cptsout,
  int *error, double *shape, int *minseglen, double *tol, double *lastchangelike,
  int *lastchangecpts, int *numchangecpts, int *MBIC){

  //data           - vectorised matrix of size n x m
  //n              - number of records
  //m              - number of data points per record
  //pen            - penalty value
  //cptsout        - position of estimated changepoints
  //error          - Index stating if an error has occured (error=0 if ok)
  //shape          - known variance (if=0,use MLE, if<0, cost=RSS)
  //minseglen      - minimum segment length
  //lastchangelike - working space (cost value at last changepoint)
  //lastchangecpts - working space (index of last changepoint)
  //numchangecpts  - working space (number of cpts that have occured so far)
  //MBIC           - 1 if MBIC penalty, 0 if not

  int p = *m - 1;   //number or regressors
  int np1 = *n + 1; //ncols of summary statistcs array
  int size = (*m * (*m + 1)) * 0.5; //nrows of summary statistics array
  int nchecklist, nchecktmp;     //number of items in the checklist
  double minout;    //minimum cost value
  int tstar, i, j, start;  //indicies
  int whichout;     //index corresponding to the minimum cost value
  double segcost;   //cost over specified segment
  *error = 0;

  void min_which(double*, int, double*, int*);

  //working space: position of last changepoint for all 'active' branches
  int *checklist = (int *)calloc(np1, sizeof(int));
  if(checklist==NULL){
    *error = 1;
    goto err1;
  }
  //working space: updated cost wrt latest observation for all 'active' branches
  double *tmplike = (double *)calloc(np1, sizeof(double));
  if(tmplike == NULL){
    *error = 2;
    goto err2;
  }
  //Summary statistcs
  double *Sumstats = (double *)calloc(np1 * size, sizeof(double));
  if(Sumstats == NULL){
    *error = 3;
    goto err3;
  }

  //Evaluate the summary statistics
  RegQuadCost_SS(data, n, m, Sumstats, &size);

  //Initialise
  for(j = 0; j <= *minseglen; j++){
    if(j==0){
      lastchangelike[j] = -*pen;
    }else{
      lastchangelike[j] = 0;
    }
    lastchangecpts[j] = 0;
    numchangecpts[j] = 0;
  }
  //Evaluate cost for second minseglen
  for(j = *minseglen+1; j <= (2 * *minseglen); j++){
    start = 0;
    RegQuadCostFunc(Sumstats, &size, &np1, &p, &start, &j, lastchangelike + j,
      tol, error, shape, MBIC);
    if(*error != 0){
      goto err4;
    }
    lastchangecpts[j] = 0;
    numchangecpts[j] = 1;
  }
  //setup check list;
  nchecklist = 2;
  checklist[0] = 0;
  checklist[1] = *minseglen+1;

  //progress through time series
  for(tstar = 2 * *minseglen+1; tstar < np1; tstar++){
    R_CheckUserInterrupt(); //Has interrupred the R session? quits if true.

    if(lastchangelike[tstar] == 0){  //Should always be true!
      for(i = 0; i < nchecklist; i++){
        //Evaluate cost&penalty based on
        // total cost&pen at last cpt + cost over current segment + penalty
        start = checklist[i];  //last point of last segment
        RegQuadCostFunc(Sumstats, &size, &np1, &p, &start, &tstar, &segcost,
          tol, error, shape, MBIC);
        if(*error != 0){
          goto err4;
        }
        tmplike[i] = lastchangelike[start] + segcost + *pen;
      }
      //Find and store branch with minimum cost&pen
      min_which(tmplike, nchecklist, &minout, &whichout);
      lastchangelike[tstar] = minout;
      lastchangecpts[tstar] = checklist[whichout];
      numchangecpts[tstar] = numchangecpts[lastchangecpts[tstar]] + 1;

      //Prune out non-minimum branches
      nchecktmp = 0;
      for(i = 0; i < nchecklist; i++){
        if(tmplike[i] <= (lastchangelike[tstar] +*pen)){
          checklist[nchecktmp] = checklist[i];
          nchecktmp++;
        }
      }
      nchecklist = nchecktmp;
    }
    //Add new cpt to checklist
    checklist[nchecklist] = tstar - *minseglen;
    nchecklist++;
  }

  //Extract optimal changepoint set
  int ncpts = 0;
  int last = *n;
  while(last != 0){
    cptsout[ncpts] = last;
    last = lastchangecpts[last];
    ncpts++;
  }

  //Free allocated memory
  err4: free(Sumstats);
  err3: free(tmplike);
  err2: free(checklist);
  err1: return;
}


//Free allocated memory in case R session has been interrupred
void Free_CptReg_Normal_PELT(int *error){
  // Error code from CptReg_Normal_PELT C function, non-zero => error
  if(*error==0){
    free((void *)checklist);
    free((void *)tmplike);
    free((void *)Sumstats);
  }
}

//Determine cpts for Normal regression under method AMOC
void CptReg_Normal_AMOC(double *data, int *n, int *m, double *pen,
  int *error, double *shape, int *minseglen, double *tol, int *tau,
  double *nulllike, double *taulike, double *tmplike, int *MBIC){

  //data           - vectorised matrix of size n x m
  //n              - number of records
  //m              - number of data points per record
  //pen            - penalty value
  //error          - Index stating if an error has occured (error=0 if ok)
  //shape          - known variance (if=0,use MLE, if<0, cost=RSS)
  //minseglen      - minimum segment length
  //tau            - estimated single changepoint position
  //nulllike       - estimated cost of no changepoints
  //taulike        - estimated cost of single changepoint
  //tmplike        - working memory: store all single changepoint costs
  //MBIC           - 1 if MBIC penalty, 0 if not

  int p = *m - 1;   //number or regressors
  int np1 = *n + 1; //ncols of summary statistcs array
  int size = (*m * (*m + 1)) * 0.5; //nrows of summary statistics array
  int tstar;
  double seg1cost, seg2cost;
  int zero;
  int neval;
  *error = 0;

  void min_which(double*, int, double*, int*);

  //Summary statistcs
  double *Sumstats = (double *)calloc(np1 * size, sizeof(double));
  if(Sumstats == NULL){
    *error = 1;
    goto err1;
  }
  //Evaluate the summary statistics
  RegQuadCost_SS(data, n, m, Sumstats, &size);

  //cost with no changepoints
  zero = 0;
  RegQuadCostFunc(Sumstats, &size, &np1, &p, &zero, n, nulllike, tol, error, shape, MBIC);
  if(*error != 0){
    goto err2;
  }

  //cost with at most one changepoint
  neval = 0;
  for(tstar = *minseglen; tstar <= (*n - *minseglen); tstar++){  //??
    R_CheckUserInterrupt(); //Has interrupred the R session? quits if true.
    RegQuadCostFunc(Sumstats, &size, &np1, &p, &zero, &tstar, &seg1cost,
      tol, error, shape, MBIC);
    if(*error != 0){
      goto err2;
    }
    RegQuadCostFunc(Sumstats, &size, &np1, &p, &tstar, n, &seg2cost,
      tol, error, shape, MBIC);
    if(*error != 0){
      goto err2;
    }
    tmplike[tstar-1] = seg1cost + seg2cost;
    neval++;
  }
  //Which tstar returns the minimum cost
  min_which(tmplike + *minseglen - 1, neval, taulike, tau);
  *tau += *minseglen;

  //Free allocated memory
  err2: free(Sumstats);
  err1: return;
}

//Free allocated memory in case R session has been interrupred
void Free_CptReg_Normal_AMOC(int *error){
  // Error code from CptReg_Normal_AMOC C function, non-zero => error
  if(*error==0){
    free((void *)Sumstats);
  }
}












