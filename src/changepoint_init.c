#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void binseg(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void FreePELT(void *);
extern void PELTC(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"binseg",   (DL_FUNC) &binseg,   10},
    {"FreePELT", (DL_FUNC) &FreePELT,  1},
    {"PELTC",     (DL_FUNC) &PELTC,     11},
    {NULL, NULL, 0}
};

void R_init_changepoint(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

