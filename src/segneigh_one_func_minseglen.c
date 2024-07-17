//#include "cost_general_functions.c"
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h> // RK addition
#include <R_ext/RS.h>  // RK addition
#include <R_ext/Lapack.h> // RK addition
#include <R_ext/BLAS.h> // RK addition
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h> // log, M_PI
#include <limits.h> // INT_MIN, INT_MAX


double get_from_dmat(double* mat, int i, int j, int w) {
	return mat[i + j * w];
}

void set_dmat(double* mat, int i, int j, int w, double val) {
	mat[i + j * w] = val;
}

int get_from_imat(int* mat, int i, int j, int w) {
	return mat[i + j * w];
}

void set_imat(int* mat, int i, int j, int w, int val) {
	mat[i + j * w] = val;
}

double* allseg_var_norm(double* data, int* n) {
	double mean = 0;
	for (int i = 0; i < *n; i++) {
		mean += data[i];
	}
	mean /= *n;

	double* all_seg = malloc((*n) * (*n) * sizeof(double));

	double m;
	double mean_diff;
	double sigmasq;
	double LOG_2_PI = log(2*M_PI);
	for (int i = 0; i < *n; i++) {
		double ssq = 0;
		for (int j = 0; j < *n; j++) {
			m = (j) - (i + 1);
			mean_diff = (data[j] - mean);
			ssq = ssq + mean_diff * mean_diff;
			if (ssq <= 0) {sigmasq = (0.00000000001/m);}
			else {sigmasq = ssq / m;}
			set_dmat(all_seg, i, j, *n, -(m/2) * (LOG_2_PI + log(sigmasq) + 1));
		}
	}

	return all_seg;
}



double* allseg_mean_norm(double* data, int* n) {

	double* all_seg = malloc((*n) * (*n) * sizeof(double));
	for (int i = 0; i < *n; i++) {
		for (int j = 0; j < *n; j++) {
			set_dmat(all_seg, i, j, *n, 0);
		}
	}

	int len;
	for (int i = 0; i < *n; i++) {
		double ssq = 0;
		double sumx = 0;
		for (int j = i; j < *n; j++) {
			len = (j - i) + 1;
			sumx = sumx + data[j];
			ssq = ssq + (data[j] * data[j]);
			set_dmat(all_seg, i, j, *n, -0.5 * (ssq - (sumx * sumx)/len));
		}
	}

	return all_seg;
}

double* allseg_meanvar_norm(double* data, int* n) {

	double* all_seg = malloc((*n) * (*n) * sizeof(double));

	int len;
	double sigmasq;
	double LOG_2_PI = log(2*M_PI);
	for (int i = 0; i < *n; i++) {
		double ssq = 0;
		double sumx = 0;
		for (int j = 0; j < *n; j++) {
			len = (j - i) + 1;
			sumx = sumx + data[j];
			ssq = ssq + (data[j] * data[j]);
			sigmasq = (1.0f/len) * (ssq - (sumx * sumx)/len);
			if (sigmasq <= 0) { sigmasq = 0.0000000001; }
			set_dmat(all_seg, i, j, *n, (-len / 2) * (LOG_2_PI + log(sigmasq) + 1));
		}
	}

	return all_seg;
}




void segneigh (char** cost_func,
			   double* data,
			   int* n,
			   int* Q,
			   double* pen,
			   int* cptsout,
			   int* error,
			   int* cps_q,
			   int* op_cps,
			   int* min_criterion,
			   double* like_q) {

	double* all_seg;
	if (strcmp(*cost_func,"var.norm")==0){
		if (*n < 4) {
			*error = 1;
			return;
		}
		all_seg = allseg_var_norm(data, n);
	}
	else if (strcmp(*cost_func,"mean.norm")==0){
		if (*n < 2) {
			*error = 1;
			return;
		}
		all_seg = allseg_mean_norm(data, n);
	}
	else if (strcmp(*cost_func,"meanvar.norm")==0){
		if (*n < 4) {
			*error = 1;
			return;
		}
		all_seg = allseg_meanvar_norm(data, n);
	}
	else {
		*error = 2;
		return;
	}

	Rprintf("cost function: %s\n", *cost_func);

	int* cp = malloc((*Q) * (*n) * sizeof(int));
	for (int i = 0; i < (*Q); i++) {
		for (int j = 0; j < (*n); j++) {
			set_imat(cp, i, j, *Q, INT_MIN);
		}
	}
	double like = 0;
	int like_index;
	double tmp = 0;
	for (int i = 0; i < *n; i++) {
		double val = get_from_dmat(all_seg, 0, i, *n);
		set_dmat(like_q, 0, i, *Q, val);
	}
	for (int q = 1; q < *Q; q++) {
		for (int j = q; j < *n; j++) {
			like = -INFINITY;
			like_index = -1;
			/// v in original algo
			for (int i = q - 1; i <= j - 1; i++) {
				tmp = get_from_dmat(like_q, q-1, i, *Q) + get_from_dmat(all_seg, i + 1, j, (*n));
				if (tmp > like) {
					like_index = i;
					like = tmp;
				}
			}
			set_dmat(like_q, q, j, (*Q), like);
			set_imat(cp, q, j, (*Q), like_index + 1);
		}
	}

	/// cps_q
	for (int q = 1; q < (*Q); q++) {
		set_imat(cps_q, q, 0, *Q, get_from_imat(cp, q, *n - 1, *Q));
		for (int i = 0; i < q; i++) {
			int value = get_from_imat(cp, q - (i + 1), get_from_imat(cps_q, q, i, (*Q)) - 1, *Q);
			set_imat(cps_q, q, i + 1, *Q, value);
		}
	}

	*min_criterion = INT_MAX;
	*op_cps = -1;
	int tmp_crit = 0;
	for (int i = 0; i < (*Q); i++) {
		tmp_crit = -2 * get_from_dmat(like_q, i, *n - 1, *Q) + i * (*pen);
		if (tmp_crit < *min_criterion) {
			*min_criterion = tmp_crit;
			*op_cps = i;
		}
	}
	if (*op_cps == -1) {
		Rprintf("Error: op.cps not set\n");
		*error = 3;
		return;
	}

	if (*op_cps == (*Q)) {
		Rprintf("The number of segments indentified is Q, it is advised to increase Q to make sure changepoints are not missed.\n");
	}

	if (*op_cps == 0) {
		cptsout[0] = (*n);
	}
	else {
		int index = 0;
		for (int i = 0; i < *Q; i++) {
			if (get_from_imat(cps_q, *op_cps, i, *Q) > 0) {
				cptsout[index++] = get_from_imat(cps_q, *op_cps, i, *Q);
			}
		}
		cptsout[index++] = *n;
		/// cptsout is sorted in R
	}


	free(all_seg);
	free(cp);
}
