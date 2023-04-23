#define ARMA_NO_DEBUG
#include "lib.h"
#include "dev.h"
#include "lda.h"
using namespace quanteda;
using namespace arma;
using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
List cpp_lda(arma::sp_mat &mt, int k, int max_iter, double alpha, double beta, double gamma,
             arma::sp_mat &seeds, arma::sp_mat &words,
             std::vector<bool> &first,
             int random, int batch, bool verbose= false, int threads = -1) {

    LDA lda(k, alpha, beta, gamma, max_iter, random, batch, verbose, threads);
    lda.set_data(mt, first);
    lda.set_fitted(words);

    if (lda.initialize() == 0) {
        bool seeded = arma::accu(seeds) > 0;
        //arma::mat s;
        if (seeded) {
            if (seeds.n_cols != lda.nw.col ||  seeds.n_rows != lda.nw.row)
                throw std::invalid_argument("Invalid seed matrix");
            Array nw_ss(seeds);
            Array nwsum_ss(arma::sum(seeds, 0));
            lda.nw += nw_ss; // set pseudo count
            //s = arma::conv_to<arma::mat>::from(arma::mat(seeds));
            //lda.nw = lda.nw + s; // set pseudo count
            //lda.nwsum = lda.nwsum + arma::sum(s, 0);
            lda.estimate();
            lda.nwsum += nwsum_ss;
            //lda.nwsum = lda.nwsum + Array(arma::sum(nw_sd, 0));
        } else {
            lda.estimate();
        }
    }
    lda.compute_theta();
    lda.compute_phi();

    return List::create(Rcpp::Named("k") = lda.K,
                        Rcpp::Named("max_iter") = lda.max_iter,
                        Rcpp::Named("last_iter") = lda.iter,
                        Rcpp::Named("alpha") = lda.alpha,
                        Rcpp::Named("beta") = lda.beta,
                        Rcpp::Named("gamma") = lda.gamma,
                        Rcpp::Named("phi") = wrap(lda.phi),
                        Rcpp::Named("theta") = wrap(lda.theta),
                        Rcpp::Named("words") = wrap(lda.nw.to_smat())); // TODO: change to zeta?
}


