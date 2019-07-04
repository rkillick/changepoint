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

static double *Sumstats;

//Determine cpts for Normal regression under method AMOC
void CptReg_Normal_AMOC(char **cost_func, double *sumstat, int *n, int *m, double *pen,
                        int *error, double *shape, int *minorder, int *optimalorder, int *maxorder, int *minseglen, double *tol, int *tau,
                        double *nulllike, double *taulike, double *tmplike, int *MBIC){

    //sumstat        - vectorised matrix of size n x m
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


    void (*costfunction)();
    void mll_reg();
    void RegQuadCost_SS();
    void min_which();

    if (strcmp(*cost_func,"regquad")==0){
     	costfunction = &mll_reg;
    }


    //Summary statistics
    double *Sumstats = (double *)calloc(np1 * size, sizeof(double));
    if(Sumstats == NULL){
        *error = 1;
        goto err1;
    }
    //Evaluate the summary statistics
    RegQuadCost_SS(sumstat, n, m, Sumstats, &size);

    //cost with no changepoints
    zero = 0;
    costfunction(Sumstats, &size, &np1, &p, minorder, optimalorder, maxorder, &zero, n, nulllike, tol, error, shape, MBIC);
    if(*error != 0){
        goto err2;
    }

    //cost with at most one changepoint
    neval = 0;
    for(tstar = *minseglen; tstar <= (*n - *minseglen); tstar++){
       R_CheckUserInterrupt(); //Has interrupted the R session? quits if true.
       costfunction(Sumstats, &size, &np1, &p, minorder, optimalorder, maxorder, &zero, &tstar, &seg1cost, tol, error, shape, MBIC);
        if(*error != 0){
            goto err2;
        }
        costfunction(Sumstats, &size, &np1, &p, minorder, optimalorder, maxorder, &tstar, n, &seg2cost, tol, error, shape, MBIC);
        if(*error != 0){
            goto err2;
        }
        tmplike[tstar-1] = seg1cost + seg2cost;
        neval++;
    }
    //Which tstar returns the minimum cost
    min_which(tmplike + *minseglen - 1, &neval, taulike, tau);
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
