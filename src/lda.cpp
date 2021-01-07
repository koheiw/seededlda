#include "lib.h"
#include "dev.h"
#include "lda.h"
using namespace quanteda;
using namespace arma;
using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
List cpp_lda(arma::sp_mat &mt, int k, int max_iter, double alpha, double beta,
             arma::sp_mat &seeds, arma::sp_mat &words, int random, bool verbose) {

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
        arma::umat s;
        if (seeded) {
            if (arma::size(seeds) != arma::size(lda.n_word_topic))
                throw std::invalid_argument("Invalid seed matrix");
            s = arma::conv_to<arma::umat>::from(arma::mat(seeds));
            lda.n_word_topic = lda.n_word_topic + s; // set pseudo count
        }
        lda.fit();
        if (seeded)
            lda.s_word_topic = lda.s_word_topic + arma::sum(s, 0);
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
                        Rcpp::Named("words") = wrap(lda.n_word_topic));
}
