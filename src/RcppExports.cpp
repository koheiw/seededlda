// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// cpp_lda
List cpp_lda(arma::sp_mat& mt, int k, int max_iter, double min_delta, std::vector<double> alpha, std::vector<double> beta, double gamma, arma::sp_mat& seeds, arma::sp_mat& words, std::vector<bool>& first, double adjust, int random, int batch, bool verbose, int threads);
RcppExport SEXP _seededlda_cpp_lda(SEXP mtSEXP, SEXP kSEXP, SEXP max_iterSEXP, SEXP min_deltaSEXP, SEXP alphaSEXP, SEXP betaSEXP, SEXP gammaSEXP, SEXP seedsSEXP, SEXP wordsSEXP, SEXP firstSEXP, SEXP adjustSEXP, SEXP randomSEXP, SEXP batchSEXP, SEXP verboseSEXP, SEXP threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat& >::type mt(mtSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< double >::type min_delta(min_deltaSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< double >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat& >::type seeds(seedsSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat& >::type words(wordsSEXP);
    Rcpp::traits::input_parameter< std::vector<bool>& >::type first(firstSEXP);
    Rcpp::traits::input_parameter< double >::type adjust(adjustSEXP);
    Rcpp::traits::input_parameter< int >::type random(randomSEXP);
    Rcpp::traits::input_parameter< int >::type batch(batchSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_lda(mt, k, max_iter, min_delta, alpha, beta, gamma, seeds, words, first, adjust, random, batch, verbose, threads));
    return rcpp_result_gen;
END_RCPP
}
// cpp_get_max_thread
int cpp_get_max_thread();
RcppExport SEXP _seededlda_cpp_get_max_thread() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(cpp_get_max_thread());
    return rcpp_result_gen;
END_RCPP
}
// cpp_tbb_enabled
bool cpp_tbb_enabled();
RcppExport SEXP _seededlda_cpp_tbb_enabled() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(cpp_tbb_enabled());
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP run_testthat_tests(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_seededlda_cpp_lda", (DL_FUNC) &_seededlda_cpp_lda, 15},
    {"_seededlda_cpp_get_max_thread", (DL_FUNC) &_seededlda_cpp_get_max_thread, 0},
    {"_seededlda_cpp_tbb_enabled", (DL_FUNC) &_seededlda_cpp_tbb_enabled, 0},
    {"run_testthat_tests", (DL_FUNC) &run_testthat_tests, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_seededlda(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
