#include "lda.h"
using namespace quanteda;
using namespace arma;
using namespace std;
using namespace Rcpp;

NumericVector to_vector(const arma::colvec& v) {
	return Rcpp::NumericVector(v.begin(), v.end());
}

// [[Rcpp::export]]
List cpp_lda(arma::sp_mat &mt, int k, int max_iter, double min_delta,
             std::vector<double> alpha, std::vector<double> beta, double gamma,
             arma::sp_mat &seeds, arma::sp_mat &words,
             std::vector<bool> &first, double adjust,
             int random, int batch, bool verbose= false, int threads = -1) {

    LDA lda(k, alpha, beta, gamma, max_iter, min_delta, adjust, random, batch, verbose, threads);
    lda.set_data(mt, first);
    lda.set_fitted(words);
    if (lda.initialize() == 0) {
        bool seeded = arma::accu(seeds) > 0;
        if (seeded) {
            if ((int)seeds.n_rows != lda.K || (int)seeds.n_cols != lda.V)
                throw std::invalid_argument("Invalid seed matrix");
            arma::mat nw_ss(seeds);
            arma::colvec nwsum_ss(arma::sum(nw_ss, 1));
            lda.nw += nw_ss; // set pseudo count
            lda.estimate(); // TODO: add LDA::set_seed
            lda.nwsum += nwsum_ss;
        } else {
            lda.estimate();
        }
    }
    lda.compute_theta(); // TODO: move to estimate()
    lda.compute_phi(); // TODO: move to estimate()

    return List::create(Rcpp::Named("k") = lda.K,
                        Rcpp::Named("max_iter") = lda.max_iter,
                        Rcpp::Named("last_iter") = lda.iter,
                        Rcpp::Named("auto_iter") = (lda.min_delta == 0),
                        Rcpp::Named("adjust_alpha") = lda.adjust,
                        Rcpp::Named("alpha") = to_vector(lda.alpha),
                        Rcpp::Named("beta") = to_vector(lda.beta),
                        Rcpp::Named("epsilon") = to_vector(lda.epsilon),
                        Rcpp::Named("gamma") = lda.gamma,
                        Rcpp::Named("phi") = wrap(lda.phi),
                        Rcpp::Named("theta") = wrap(lda.theta),
                        Rcpp::Named("words") = wrap(arma::sp_mat(lda.nw.t()))); // NOTE: change to zeta?
}


