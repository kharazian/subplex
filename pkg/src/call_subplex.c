// Dear Emacs, treat this as -*- C++ -*-

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>

typedef double _subplex_objective_function (int *, double *);

SEXP call_subplex(SEXP x, SEXP f, SEXP tol, SEXP maxnfe, SEXP scale, SEXP rho, SEXP args);
void F77_NAME(subplx) (_subplex_objective_function *f, int *n, double *tol, int *maxnfe, int *mode,
		       double *scale, double *x, double *fx, int *nfe, double *work, int *iwork, 
		       int *iflag);
static double default_subplex_objective(int *n, double *x);

// these global objects will pass the needed information to the user-defined function
SEXP _subplex_Xvec; // vector of arguments (allocated once, refilled many times)
SEXP _subplex_envir;	  // environment in which function was defined
SEXP _subplex_fcall;	      // function call (constructed just once)

// the wrapper around the user's objective function.
// will be called by the Fortran routine "subplx".
static double default_subplex_objective (int *n, double *x) 
{
  double *xp, retval;
  int k;
  SEXP ans;
  R_CheckUserInterrupt();
  xp = REAL(_subplex_Xvec);
  for (k = 0; k < *n; k++) xp[k] = x[k]; // copy the values
  PROTECT(ans = eval(_subplex_fcall,_subplex_envir)); // evaluate the call
  retval = NUMERIC_VALUE(ans);	// extract the function value
  UNPROTECT(1);
  return retval;
}

SEXP call_subplex (SEXP x, SEXP f, SEXP tol, SEXP maxnfe, SEXP scale, SEXP rho, SEXP args) 
{
  int nprotect = 0;
  double *work, *scalp, *xp, *Xp;
  int n, *iwork, mode = 0;
  int k, nscal;
  _subplex_objective_function *objfn = default_subplex_objective;
  SEXP ans, ansnames, X, Xnames, val, counts, conv, fn, arglist;

  n = LENGTH(x);
  PROTECT(x = AS_NUMERIC(x)); nprotect++;

  if (!isFunction(f)) {
    UNPROTECT(nprotect);
    error("'f' must be a function");
  }

  // check the convergence tolerance, tol
  PROTECT(tol = AS_NUMERIC(tol)); nprotect++;
  if ((LENGTH(tol) > 1) || (NUMERIC_VALUE(tol) < 0.0)) {
    UNPROTECT(nprotect);
    error("'tol' must be a non-negative scalar");
  }

  // check the maximum number of function evaluations, maxnfe
  PROTECT(maxnfe = AS_INTEGER(maxnfe)); nprotect++;
  if (INTEGER_VALUE(maxnfe) <= 0) {
    UNPROTECT(nprotect);
    error("'maxnfe' must be a positive integer");
  }

  // process the scale and first step parameters, scale
  nscal = LENGTH(scale);
  if ((nscal > 1) && (nscal != n)) {
    UNPROTECT(nprotect);
    error("'scale' misspecified: either specify a single scale or one for each component of 'par'");
  }
  PROTECT(scale = AS_NUMERIC(scale)); nprotect++;
  scalp = REAL(scale);
  if (nscal == 1) { // peculiarity of subplex: if scale < 0, assume there is one scale for all components
    scalp[0] = -fabs(scalp[0]);
  } else {
    for (k = 0; k < nscal; k++) scalp[k] = fabs(scalp[k]);
  }

  PROTECT(fn=f); nprotect++;
  PROTECT(Xnames=GET_NAMES(x)); nprotect++; // get the names attribute
  PROTECT(X = NEW_NUMERIC(n)); nprotect++; // allocate a vector for passing to subplx and holding the return values
  PROTECT(_subplex_Xvec = NEW_NUMERIC(n)); nprotect++; // for internal use
  SET_NAMES(X,Xnames); // make sure the names attribute is copied to the return vector
  SET_NAMES(_subplex_Xvec,Xnames); // make sure the names attribute shows up in each call to the user function
  PROTECT(_subplex_envir=rho); nprotect++; // store the function's environment
  PROTECT(arglist = CONS(_subplex_Xvec,args)); nprotect++; // prepend Xvec onto the argument list
  PROTECT(_subplex_fcall = LCONS(fn,arglist)); nprotect++; // set up the function call
  
  PROTECT(val = NEW_NUMERIC(1)); nprotect++; // to hold the objective function value
  PROTECT(counts = NEW_INTEGER(1)); nprotect++;	// to count the number of function evaluations
  PROTECT(conv = NEW_INTEGER(1)); nprotect++; // to hold the convergence code

  // the following memory allocation is based on an interpretation of the subplex documentation
  // the memory *should* be reclaimed by R upon return from .Call
  // I HAVE NOT VERIFIED THAT IT IS CORRECT.
  if (
      !(work = (double *) R_alloc(n*(n+6)+1,sizeof(double))) ||
      !(iwork = (int *) R_alloc(2*n,sizeof(int)))
      ) {
    UNPROTECT(nprotect);
    error("'par' too long, insufficient memory available");
  }

  xp = REAL(x); Xp = REAL(X);
  for (k = 0; k < n; k++) Xp[k] = xp[k]; // copy in the initial guess vector

  F77_CALL(subplx)(objfn,&n,REAL(tol),INTEGER(maxnfe),&mode,scalp,Xp,REAL(val),INTEGER(counts),
		   work,iwork,INTEGER(conv));

  if (INTEGER_VALUE(conv) == -2) {
    UNPROTECT(nprotect);
    error("illegal input in subplex");
  }

  PROTECT(ansnames = NEW_CHARACTER(4)); nprotect++;
  SET_STRING_ELT(ansnames,0,mkChar("par"));
  SET_STRING_ELT(ansnames,1,mkChar("value"));
  SET_STRING_ELT(ansnames,2,mkChar("counts"));
  SET_STRING_ELT(ansnames,3,mkChar("convergence"));
  PROTECT(ans = NEW_LIST(4)); nprotect++;
  SET_NAMES(ans,ansnames);
  SET_VECTOR_ELT(ans,0,X);
  SET_VECTOR_ELT(ans,1,val);
  SET_VECTOR_ELT(ans,2,counts);
  SET_VECTOR_ELT(ans,3,conv);

  UNPROTECT(nprotect);
  return ans;
}
