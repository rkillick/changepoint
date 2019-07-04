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

//#include "cost_general_functions.c"
#define SWAP(a,b)   { int t; t=a; a=b; b=t; }  // Macro for swapping

static int *checklist;
static double *tmplike;
static int *tmpt;
static double *Sumstats;

void FreePELT(error)
	int *error; /* Error code from PELT C function, non-zero => error */
	{
	if(*error==0){
	  free((void *)checklist);
	  free((void *)tmplike);
	  free((void *)tmpt);
    free((void *)Sumstats);
  }
}

void PELT(cost_func, sumstat, n, m, pen, cptsout, error, shape, minorder, optimalorder, maxorder, minseglen, tol, lastchangelike, lastchangecpts, numchangecpts, MBIC)
  char **cost_func;
  double *sumstat;    /* Summary statistic for the time series, vectorised matrix of size n x m */
  int *n;			/* Length of the time series */
  int *m;     /* number of dimensions (regressors+1) */
  double *pen;  /* Penalty used to decide if a changepoint is significant */
  int *cptsout;    /* Vector of identified changepoint locations */
  int *error;   /* 0 by default, nonzero indicates error in code */
  double *shape; // only used when cost_func is the gamma likelihood
  int *minorder; /* minimum order for mll_ar */
  int *optimalorder; /* vector of optimal orders per segment */
  int *maxorder; /* maximum order for mll_ar */
  int *minseglen; //minimum segment length
  double *tol;    // tolerance only for cpt.reg
  double *lastchangelike; // stores likelihood up to that time using optimal changepoint locations up to that time
  int *lastchangecpts; // stores last changepoint locations
  int *numchangecpts; //stores the current number of changepoints
  int *MBIC;          // 1 if MBIC penalty, 0 if not
  {
	// R code does know.mean and fills mu if necessary

	  int p = *m - 1;   //number or regressors
	  int np1 = *n + 1; // length of time series +1 for convenience
	  int size = (*m * (*m + 1)) * 0.5; //nrows of summary statistics array
	  int nchecklist, nchecktmp;     //number of items in the checklist
	  int start;  //indicies
	  double segcost=0;   //cost over specified segment
	  *error = 0;
	  double minval; // previously minout
	  int tstar,i,j,minid; // minid previously whichout


void (*costfunction)();
void mean_norm();
void var_norm();
void meanvar_norm();
void meanvar_exp();
void meanvar_gamma();
void meanvar_poisson();
void mll_reg();
//void mll_ar();


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
else if (strcmp(*cost_func,"regquad")==0){
 	costfunction = &mll_reg;
}
//else if (strcmp(*cost_func,"ar.norm")==0){
//	costfunction = &mll_ar; //change to ARP() when done but leave like this for now to stop errors when running
//}


	int *checklist;
	checklist = (int *)calloc(np1, sizeof(int));
	if(checklist==NULL){
		*error = 1;
		goto err1;
	}


//double tmplike[*n];
  double *tmplike;
  tmplike = (double *)calloc(np1,sizeof(double));
  if (tmplike==NULL){
    *error = 2;
    goto err2;
  }


  //int tmpt[*n];
  int *tmpt;
  tmpt = (int *)calloc(np1,sizeof(int));
  if (tmpt==NULL){
    *error = 3;
    goto err3;
  }

  void min_which();

	if (strcmp(*cost_func,"regquad")==0){
	  // The sumstat from R for reg is just the design matrix so we need to calculate the actual summary statistics
	  //Summary statistics

	  double *Sumstats;
	  Sumstats = (double *)calloc(np1 * size, sizeof(double));
	  if(Sumstats == NULL){
	    *error = 4;
	    goto err4;
	  }

	  void RegQuadCost_SS();
	  RegQuadCost_SS(sumstat, n, m, Sumstats, &size);
      sumstat = Sumstats; // updates sumstat to be the actual summary statistics
	}

  //Initialise
  for(j = 0; j < *minseglen; j++){
    if(j==0){
      lastchangelike[j] = -*pen;
    }else{
      lastchangelike[j] = 0;
    }
    lastchangecpts[j] = 0;
    numchangecpts[j] = 0;
  }

 //Evaluate cost for second minseglen
  start = 0;
	for(j = *minseglen; j < (2 * *minseglen); j++){
      costfunction(sumstat, &size, &np1, &p, minorder, optimalorder, maxorder, &start, &j, &segcost, tol, error, shape, MBIC);
      lastchangelike[j]=segcost;

			if(strcmp(*cost_func,"regquad")==0){
        if(*error != 0){
            goto err5;
        }
			}else{
				if(*error != 0){
            goto err4;
        }
			}
      lastchangecpts[j] = 0;
      numchangecpts[j] = 1;
    }

//setup checklist
  nchecklist=2;
  checklist[0]=0;
  checklist[1]=*minseglen;

  for(tstar=2 * (*minseglen); tstar < np1;tstar++){
    R_CheckUserInterrupt(); // checks if user has interrupted the R session and quits if true

   if ((lastchangelike[tstar]) == 0){
  		for(i=0;i<(nchecklist);i++){
				start = checklist[i];  //last point of last segment
				costfunction(sumstat, &size, &np1, &p, minorder, optimalorder, maxorder, &start, &tstar, &segcost, tol, error, shape, MBIC);

				if(strcmp(*cost_func,"regquad")==0){
	        if(*error != 0){
	            goto err5;
	        }
				}else{
					if(*error != 0){
	            goto err4;
	        }
				}
				tmplike[i] = lastchangelike[start] + segcost + *pen;
      }

    min_which(tmplike,&nchecklist,&minval,&minid); // updates minval and minid with min and which element 
    lastchangelike[tstar]=minval;
    lastchangecpts[tstar]=checklist[minid];
    numchangecpts[tstar]=numchangecpts[lastchangecpts[tstar]]+1;

    // Update checklist for next iteration, first element is next tau
      nchecktmp=0;
			for(i = 0; i < nchecklist; i++){
				if(tmplike[i] <= (lastchangelike[tstar] +*pen)){
					checklist[nchecktmp] = checklist[i];
					nchecktmp++;
				}
			}
			nchecklist = nchecktmp;
		}
		//Add new cpt to checklist
		checklist[nchecklist] = tstar - (*minseglen-1); // atleast 1 obs per seg
		nchecklist++;

    //  nchecklist=nchecktmp;

  } // end taustar

  // put final set of changepoints together
  int ncpts=0;
  int last=*n;
	while(last != 0){
        cptsout[ncpts] = last;
        last = lastchangecpts[last];
        ncpts++;
    }
	
	err5:  free(Sumstats);
	err4:  free(tmpt);
	err3:  free(tmplike);
	err2:  free(checklist);
 // err3:  free(lastchangelike);
 // err2:  free(lastchangecpts);
  err1:  return;

}
