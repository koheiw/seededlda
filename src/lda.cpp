//#define ARMA_NO_DEBUG
#include "lib.h"
#include "dev.h"
#include "lda.h"
using namespace quanteda;
using namespace arma;
using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
List cpp_lda(arma::sp_mat &mt, int k, int max_iter, double alpha, double beta,
             arma::sp_mat &seeds, arma::sp_mat &words, int random,
             int parallel = 0, bool verbose = false) {

    mt = mt.t();
    seeds = seeds.t();
    words = words.t();

    LDA lda;
    lda.K = k;
    lda.set_data(mt);
    lda.set_fitted(words);
    lda.random = random;
    if (max_iter > 0)
        lda.niters = max_iter;
    if (alpha > 0)
        lda.alpha = alpha;
    if (beta > 0)
        lda.beta = beta;
    if (verbose)
        lda.verbose = verbose;
    if (lda.init_est() == 0) {
        bool seeded = arma::accu(seeds) > 0;
        arma::mat s;
        if (seeded) {
            if (arma::size(seeds) != arma::size(lda.nw))
                throw std::invalid_argument("Invalid seed matrix");
            s = arma::mat(seeds);
            lda.nw = lda.nw + s; // set pseudo count
            //lda.nwsum = lda.nwsum + arma::sum(s, 0);
        }
        lda.fit(parallel);
        if (seeded)
            lda.nwsum = lda.nwsum + arma::colvec(arma::sum(s, 1));
    }
    lda.compute_theta();
    lda.compute_phi();

    return List::create(Rcpp::Named("k") = lda.K,
                        Rcpp::Named("max_iter") = lda.niters,
                        Rcpp::Named("last_iter") = lda.liter,
                        Rcpp::Named("alpha") = lda.alpha,
                        Rcpp::Named("beta") = lda.beta,
                        Rcpp::Named("phi") = wrap(lda.phi),
                        Rcpp::Named("theta") = wrap(lda.theta),
                        Rcpp::Named("words") = wrap(lda.nw));
}
