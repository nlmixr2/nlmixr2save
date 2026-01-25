#include <Rcpp.h>

//' Fix nlmixr2 save output
//'
//' This function modifies the output of nlmixr2's save function to ensure that
//' the "Estimate" and "SE" fields are numeric vectors with appropriate names,
//' rather than data frames. This is necessary for compatibility with other
//' functions that expect these fields to be numeric vectors.
//'
//' @param obj A list object returned by nlmixr2's save function.
//'
//' @return A modified data.frame object with "Estimate" and "SE" as named numeric vectors
//'
//' @keywords internal
//'
//' @export
//'
// [[Rcpp::export]]
SEXP nlmixr2saveParFixedDf(SEXP obj) {
  // This makes the data.frame types match what nlmixr2 outputs
  Rcpp::List ret = Rcpp::as<Rcpp::List>(obj);
  Rcpp::NumericVector nv;
  if (ret.containsElementNamed("Estimate")) {
    nv = ret["Estimate"];
    Rf_setAttrib(nv, R_NamesSymbol, Rf_getAttrib(obj, R_RowNamesSymbol));
    ret["Estimate"] = nv;
  }
  if (ret.containsElementNamed("SE")) {
    nv = ret["SE"];
    Rf_setAttrib(nv, R_NamesSymbol, Rf_getAttrib(obj, R_RowNamesSymbol));
    ret["SE"] = nv;
  }
  Rf_setAttrib(ret, R_RowNamesSymbol, Rf_getAttrib(obj, R_RowNamesSymbol));
  ret.attr("class") = "data.frame";
  return Rcpp::wrap(ret);
}
