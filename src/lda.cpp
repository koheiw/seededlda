#include "lib.h"
#include "dev.h"
#include "lda.h"
using namespace quanteda;
using namespace arma;
using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
List cpp_lda(arma::sp_mat &mt, int k, int max_iter, double min_delta,
             std::vector<double> alpha, std::vector<double> beta, double gamma,
             arma::sp_mat &seeds, arma::sp_mat &words,
             std::vector<bool> &first,
             int random, int batch, bool verbose= false, int threads = -1) {

    LDA lda(k, alpha, beta, gamma, max_iter, min_delta, random, batch, verbose, threads);
    lda.set_data(mt, first);
    lda.set_fitted(words);

    if (lda.initialize() == 0) {
        bool seeded = arma::accu(seeds) > 0;
        if (seeded) {
            if (seeds.n_cols != lda.nw.col ||  seeds.n_rows != lda.nw.row)
                throw std::invalid_argument("Invalid seed matrix");
            Array nw_ss(seeds);
            Array nwsum_ss(arma::sum(seeds, 0));
            lda.nw += nw_ss; // set pseudo count
            lda.estimate();
            lda.nwsum += nwsum_ss;
        } else {
            lda.estimate();
        }
    }
    lda.compute_theta();
    lda.compute_phi();

    return List::create(Rcpp::Named("k") = lda.K,
                        Rcpp::Named("max_iter") = lda.max_iter,
                        Rcpp::Named("last_iter") = lda.iter,
                        Rcpp::Named("auto_iter") = (lda.min_delta == 0),
                        Rcpp::Named("alpha") = lda.alpha,
                        Rcpp::Named("beta") = lda.beta,
                        Rcpp::Named("gamma") = lda.gamma,
                        Rcpp::Named("phi") = wrap(lda.phi),
                        Rcpp::Named("theta") = wrap(lda.theta),
                        Rcpp::Named("words") = wrap(lda.nw.to_smat())); // TODO: change to zeta?
}


