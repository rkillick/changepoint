#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void binseg(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void FreePELT(void *);
extern void PELTC(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void CptReg_Normal_PELT(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void Free_CptReg_Normal_PELT(void *);
extern void CptReg_Normal_AMOC(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void Free_CptReg_Normal_AMOC(void *);

static const R_CMethodDef CEntries[] = {
    {"binseg",   (DL_FUNC) &binseg,   10},
    {"FreePELT", (DL_FUNC) &FreePELT,  1},
    {"PELTC",     (DL_FUNC) &PELTC,     11},
    {"CptReg_Normal_PELT", (DL_FUNC) &CptReg_Normal_PELT, 13},
    {"Free_CptReg_Normal_PELT", (DL_FUNC) &Free_CptReg_Normal_PELT, 1},
    {"CptReg_Normal_AMOC", (DL_FUNC) &CptReg_Normal_AMOC, 13},
    {"Free_CptReg_Normal_AMOC", (DL_FUNC) &Free_CptReg_Normal_AMOC,1},
    {NULL, NULL, 0}
};

void R_init_changepoint(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

