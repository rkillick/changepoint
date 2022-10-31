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
	    double* pen, //Penalty used to decide if a changepoint is significant 
	    int* Q, //Max number of changepoints
	    int* cptsout, //Q length vector of identified changepoint locations 
	    int* minseglen, //minimum segment length 
	    double* likeout, //Q length vector of likelihood ratio values for changepoints in cptsout 
	    int* op_cps, //Optimal number of changepoint for pen supplied 
	    double* shape //only used when cost_func is the gamma likelihood 
	    )
{  
  double oldmax=1.7E+308,null,lambda[*n],maxout;
  int q,p,i,j,whichout,st,end;
  for(i=0;i<*n;i++){
    lambda[i]=-INFINITY;
  }
  int tau[*Q+2]; // max ncpts is Q, +2 is for 0 and n
  tau[0]=0;
  tau[1]= *n;
     
  double mll_var(double, double, double, int, double); 
  double mll_mean(double, double, double, int, double); 
  double mll_meanvar(double, double, double, int, double); 
  double mll_meanvar_exp(double, double, double, int, double); 
  double mll_meanvar_gamma(double, double, double, int, double); 
  double mll_meanvar_poisson(double, double, double, int, double); 
  double mbic_var(double, double, double, int, double); 
  double mbic_mean(double, double, double, int, double); 
  double mbic_meanvar(double, double, double, int, double); 
  double mbic_meanvar_exp(double, double, double, int, double); 
  double mbic_meanvar_gamma(double, double, double, int, double); 
  double mbic_meanvar_poisson(double, double, double, int, double); 

  double (*costfunction)();
  if (strcmp(*cost_func,"var.norm")==0){
    costfunction = &mll_var;
  }
  else if (strcmp(*cost_func,"mean.norm")==0){
    costfunction = &mll_mean;
  }  
  else if (strcmp(*cost_func,"meanvar.norm")==0){
    costfunction = &mll_meanvar;
  }
  else if (strcmp(*cost_func,"meanvar.exp")==0){
    costfunction = &mll_meanvar_exp;
  }
  else if (strcmp(*cost_func,"meanvar.gamma")==0){
    costfunction = &mll_meanvar_gamma;
  }
  else if (strcmp(*cost_func,"meanvar.poisson")==0){
    costfunction = &mll_meanvar_poisson;
  }
  else if (strcmp(*cost_func,"mean.norm.mbic")==0){
    costfunction = &mbic_mean;
  }
  else if (strcmp(*cost_func,"var.norm.mbic")==0){
    costfunction = &mbic_var;
  }
  else if (strcmp(*cost_func,"meanvar.norm.mbic")==0){
    costfunction = &mbic_meanvar;
  }
  else if (strcmp(*cost_func,"meanvar.exp.mbic")==0){
    costfunction = &mbic_meanvar_exp;
  }
  else if (strcmp(*cost_func,"meanvar.gamma.mbic")==0){
    costfunction = &mbic_meanvar_gamma;
  }
  else if (strcmp(*cost_func,"meanvar.poisson.mbic")==0){
    costfunction = &mbic_meanvar_poisson;
  } 

  void max_which();
  void order_vec();

  for(q=0;q<*Q;q++){
    R_CheckUserInterrupt(); // checks if user has interrupted the R session and quits if true 
						for(p=0;p<*n;p++){lambda[p]=0;}
    i=1;
    st=tau[0]+1;
    end=tau[1];
    //null= (-0.5) * mll_var(*(y2+end)-*(y2+st-1),end-st+1);
    //  null = (-0.5)* call_function(*cost_function,n,sumstat,end,st-1,end-(st-1),*shape); 
    null = (-0.5)*costfunction(*(sumstat+end)-*(sumstat+st-1),*(sumstat + *n + 1 +end)-*(sumstat + *n + st),*(sumstat + *n + *n + 2 +end)-*(sumstat + *n + *n + 1 +st), end - st + 1, *shape);
    for(j=2;j<(*n-2);j++){
      if(j==end){
	st=end+1;
	i=i+1;
	end=tau[i];
	//null= (-0.5) * mll_var(*(y2+end)-*(y2+st-1),end-st+1);
	// null = (-0.5)* call_function(*cost_function,n,sumstat,end,st-1,end-(st-1),*shape);
	null = (-0.5)* costfunction(*(sumstat+end)-*(sumstat+st-1),*(sumstat + *n + 1 +end)-*(sumstat + *n + st),*(sumstat + *n + *n + 2 +end)-*(sumstat + *n + *n + 1 +st), end - st + 1, *shape);
      }
      else{
	if(((j-st)>=*minseglen)&&((end-j)>=*minseglen)){
	  // lambda[j]= ((-0.5) * mll_var(*(y2+j)-*(y2+st-1),j-st+1)) + ((-0.5) * mll_var(*(y2+end)-*(y2+j),end-j)) - null; 
	  //lambda[j] =  ((-0.5)*call_function(*cost_function,n,sumstat,j,st-1,j-(st-1),*shape)) + ((-0.5)*call_function(*cost_function,n,sumstat,end,j,end-j,*shape)) - null;
	  lambda[j] =  ((-0.5)*costfunction(*(sumstat+j)-*(sumstat+st-1),*(sumstat + *n + 1 +j)-*(sumstat + *n + st),*(sumstat + *n + *n + 2 +j)-*(sumstat + *n + *n + 1 +st), j - st + 1, *shape)) + 
	    ((-0.5)*costfunction(*(sumstat+end)-*(sumstat+j),*(sumstat + *n + 1 +end)-*(sumstat + *n + j + 1),*(sumstat + *n + *n + 2 +end)-*(sumstat + *n + *n + 2 +j), end - j, *shape)) - null;
	}
      }
    }

    max_which(lambda,*n,&maxout,&whichout);

    *(cptsout+q)=whichout;
    *(likeout+q)= (oldmax<=maxout) ? oldmax : maxout ;
    oldmax= *(likeout+q);
    tau[q+2]=whichout;
    order_vec(tau,q+3);
  }

  int stop=0;
  *op_cps=0;
  while((stop==0)&&(*op_cps < *Q)){
    if((2* *(likeout+*op_cps)) >= *pen){ (*op_cps)++; }
    else{ stop=1; }
  }
}


// Cost functions  

   /*
     double mll_var(double *sumstat, int end, int start, int n, double shape){
     double x3=*(sumstat+end)-*(sumstat+start);
     if(x3<=0){x3=0.00000000001;}
     return(n*(log(2*M_PI)+log(x3/n)+1)); // M_PI is in Rmath.h  
     }

     double mll_meanvar(double *sumstat, int end, int start, int n, double shape){
     double x2=*(sumstat+n+1+end)-*(sumstat+n+1+start); // this relies on the R code doing things in the correct order!
     double x=*(sumstat+end)-*(sumstat+start);
     double sigsq=(x2-((x*x)/n))/n;
     if(sigsq<=0){sigsq=0.00000000001;}
     return(n*(log(2*M_PI)+log(sigsq)+1)); // M_PI is in Rmath.h  
     }


     double mll_mean(double *sumstat, int end, int start, int n, double shape){
     double x2=*(sumstat+n+1+end)-*(sumstat+n+1+start); // this relies on the R code doing things in the correct order!
     double x=*(sumstat+end)-*(sumstat+start);
     return(x2-(x*x)/n);
     }

     double mll_meanvar_exp(double *sumstat, int end, int start, int n, double shape){
     double x=*(sumstat+end)-*(sumstat+start);
     return(2*n*(log(x)-log(n)));
     }

     double mll_meanvar_gamma(double *sumstat, int end, int start, int n, double shape){
     double x=*(sumstat+end)-*(sumstat+start);
     return(2*n*shape*(log(x)-log(n*shape)));
     }

     double mll_meanvar_poisson(double *sumstat, int end, int start, int n, double shape){
     double x=*(sumstat+end)-*(sumstat+start);
     if(x==0){return(0);}
     else{return(2*x*(log(n)-log(x)));}
     }


     // code to choose cost function  

     const static struct {
     char *name;
     double (*func)(double *sumstat, int end, int start, int n, double shape);
     } function_map [] = {
     { "norm.var", mll_var},
     {"norm.mean", mll_mean},
     {"norm.meanvar", mll_meanvar},
     {"exp", mll_meanvar_exp},
     {"gamma", mll_meanvar_gamma},
     {"poisson", mll_meanvar_poisson},
     };

     double call_function(const char *name,double *sumstat, int end, int start, int n, double shape)
     {
     int k;

     for (k = 0; k <= (sizeof(function_map) / sizeof(function_map[0])); k++) {
     if (!strcmp(function_map[k].name, name) && function_map[k].func) {
     return function_map[k].func(sumstat,end,start, n, shape);
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

     void order_vec( int a[], int n ){   
     int i, j;
     for(i = 0; i < n; i++){         // Make a pass through the array for each element
     for(j = 1; j < (n-i); j++){			// Go through the array beginning to end
     if(a[j-1] > a[j])       // If the the first number is greater, swap it 
     SWAP(a[j-1],a[j]);   
     }
     }
     }
   */
