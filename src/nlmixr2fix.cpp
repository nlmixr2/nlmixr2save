#include <Rcpp.h>

//' Fix nlmixr2 save output
//'
//' This function modifies the output of nlmixr2's save function to ensure that
//' the numeric columns of the `parFixedDf` data frame have the types that
//' nlmixr2 outputs after a CSV round-trip (all-`NA` columns are read back as
//' logical vectors, so they are coerced back to numeric here).
//'
//' Depending on the version of nlmixr2est that created the fit, the
//' "Estimate" and "SE" columns are either named numeric vectors (names
//' matching the row names; nlmixr2est <= 6.0) or plain unnamed numeric
//' vectors (the `$parFixed` refactor in newer nlmixr2est).  The `named`
//' argument selects which structure is restored; `nlmixr2save::saveFit()`
//' records the correct value in the generated restore script based on the
//' fit being saved.
//'
//' @param obj A list object returned by nlmixr2's save function.
//'
//' @param named Logical; when `TRUE` (default, matching fits from older
//'   nlmixr2est) the "Estimate" and "SE" columns are named using the row
//'   names; when `FALSE` they are left as unnamed numeric vectors.
//'
//' @return A modified data.frame object with numeric columns restored to
//'   the structure of the original fit
//'
//' @keywords internal
//'
//' @export
//'
// [[Rcpp::export]]
SEXP nlmixr2saveParFixedDf(SEXP obj, bool named = true) {
  // This makes the data.frame types match what nlmixr2 outputs
  Rcpp::List ret = Rcpp::as<Rcpp::List>(obj);
  Rcpp::NumericVector nv;
  const char *nameCols[] = {"Estimate", "SE"};
  for (int i = 0; i < 2; i++) {
    if (ret.containsElementNamed(nameCols[i])) {
      nv = Rcpp::as<Rcpp::NumericVector>(ret[nameCols[i]]);
      if (named) {
        Rf_setAttrib(nv, R_NamesSymbol, Rf_getAttrib(obj, R_RowNamesSymbol));
      } else {
        Rf_setAttrib(nv, R_NamesSymbol, R_NilValue);
      }
      ret[nameCols[i]] = nv;
    }
  }
  const char *numCols[] = {"%RSE", "Back-transformed", "CI Lower", "CI Upper",
                           "BSV(SD)", "BSV(CV%)", "BSV(CV% or SD)",
                           "Shrink(SD)%"};
  for (int i = 0; i < 8; i++) {
    if (ret.containsElementNamed(numCols[i])) {
      nv = Rcpp::as<Rcpp::NumericVector>(ret[numCols[i]]);
      ret[numCols[i]] = nv;
    }
  }
  Rf_setAttrib(ret, R_RowNamesSymbol, Rf_getAttrib(obj, R_RowNamesSymbol));
  ret.attr("class") = "data.frame";
  return Rcpp::wrap(ret);
}
