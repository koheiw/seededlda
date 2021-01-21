/*
 * Copyright (C) 2007 by
 *
 * 	Xuan-Hieu Phan
 *	hieuxuan@ecei.tohoku.ac.jp or pxhieu@gmail.com
 * 	Graduate School of Information Sciences
 * 	Tohoku University
 *
 * Copyright (C) 2020 by
 *
 * 	Kohei Watanabe
 * 	watanabe.kohei@gmail.com
 *
 * GibbsLDA++ is a free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2 of the License,
 * or (at your option) any later version.
 *
 * GibbsLDA++ is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GibbsLDA++; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

/*
 * References:
 * + The Java code of Gregor Heinrich (gregor@arbylon.net)
 *   http://www.arbylon.net/projects/LdaGibbsSampler.java
 * + "Parameter estimation for text analysis" by Gregor Heinrich
 *   http://www.arbylon.net/publications/text-est.pdf
 */

#include "lib.h"
#include "dev.h"

using namespace std;
using namespace Rcpp;
using namespace quanteda;

// LDA model
class LDA {
    public:
        // --- model parameters and variables ---
        int M; // dataset size (i.e., number of docs)
        int V; // vocabulary size
        int K; // number of topics
        double alpha, beta; // LDA hyperparameters
        int niters; // number of Gibbs sampling iterations
        int liter; // the iteration at which the model was saved
        int random; // seed for random number generation
        bool verbose; // print progress messages

        bool seeded;
        bool fitted;

        arma::sp_mat data; // transposed document-feature matrix
        arma::colvec p; // temp variable for sampling
        Texts topics; // topic assignments for words, size M x doc.size()
        Texts texts;
        arma::mat nw; // cwt[i][j]: number of instances of word/term i assigned to topic j, size V x K
        arma::mat nd; // na[i][j]: number of words in document i assigned to topic j, size M x K
        arma::colvec nwsum; // nwsum[j]: total number of words assigned to topic j, size K
        arma::colvec ndsum; // nasum[i]: total number of words in document i, size M
        arma::mat theta; // theta: document-topic distributions, size M x K
        arma::mat phi; // phi: topic-word distributions, size K x V

        // prediction with fitted model
        arma::mat nw_ft;
        arma::colvec nwsum_ft;

        // random number generators
        std::default_random_engine generator;
        std::uniform_real_distribution<double> random_prob;
        std::uniform_int_distribution<int> random_topic;

        // --------------------------------------

        LDA() {
    	    set_default_values();
        }

        // set default values for variables
        void set_default_values();
        void set_data(arma::sp_mat mt);
        void set_fitted(arma::sp_mat mt);

        // init for estimation
        int init_est();

        // estimate LDA model using Gibbs sampling
        void fit(bool parallel);
        struct estimateWorker;
        void estimate(int m);
        void estimate();
        int sample(int m, int n, int w);
        int sample(int m, int n, int w,
                   arma::mat& nw_tp, arma::mat& nd_tp, arma::colvec& nwsum_tp);
        void compute_theta();
        void compute_phi();

};

void LDA::set_default_values() {

    M = 0;
    V = 0;
    K = 100;
    alpha = 50.0 / K;
    beta = 0.1;
    niters = 2000;
    liter = 0;
    verbose = false;
    random = 1234;
    seeded = false;
    fitted = false;
}

void LDA::set_data(arma::sp_mat mt) {

    data = mt;
    M = data.n_cols;
    V = data.n_rows;
    //printf("M = %d, V = %d\n", M, V);
}

void LDA::set_fitted(arma::sp_mat words) {

    if ((int)words.n_rows != K || (int)words.n_cols != V)
        throw std::invalid_argument("Invalid word matrix");
    nw_ft = arma::mat(words);
    nwsum_ft = arma::colvec(arma::mat(arma::sum(nw_ft, 1)));

}

int LDA::init_est() {

    if (verbose) {
        Rprintf("Fitting LDA with %d topics\n", K);
        Rprintf("   ...initializing\n");
    }

    std::default_random_engine generator(random);
    std::uniform_real_distribution< double > random_prob(0, 1);
    std::uniform_int_distribution< int > random_topic(0, K - 1);

    topics = Texts(M);
    texts = Texts(M);

    p = arma::colvec(K);
    theta = arma::mat(M, K, arma::fill::zeros);
    phi = arma::mat(K, V, arma::fill::zeros);

    nw = arma::mat(K, V, arma::fill::zeros);
    nd = arma::mat(K, M, arma::fill::zeros);
    nwsum = arma::colvec(K, arma::fill::zeros);

    ndsum = arma::colvec(arma::mat(arma::sum(data, 0).t()));

    //dev::Timer timer;
    //dev::start_timer("Set z", timer);
    for (int m = 0; m < M; m++) {

        topics[m] = Text(ndsum[m]);
        texts[m] = Text(ndsum[m]);

        if (topics[m].size() == 0) continue;
        int n = 0;

        arma::sp_mat::const_col_iterator it = data.begin_col(m);
        arma::sp_mat::const_col_iterator it_end = data.end_col(m);
        for(; it != it_end; ++it) {
            int w = it.row();
            int F = *it;
            for (int f = 0; f < F; f++) {
                int topic = random_topic(generator);
                topics[m][n] = topic;
                texts[m][n] = w;
                // number of instances of word i assigned to topic j
                nw.at(topic, w) += 1;
                // number of words in document i assigned to topic j
                nd.at(topic, m) += 1;
                // total number of words assigned to topic j
                nwsum[topic] += 1;
                n++;
            }
        }
    }
    //dev::stop_timer("Set z", timer);

    return 0;
}

void LDA::fit(bool parallel) {

    if (verbose)
        Rprintf("   ...Gibbs sampling in %d itterations\n", niters);

    int last_iter = liter;
    for (liter = last_iter + 1; liter <= niters + last_iter; liter++) {

        if (liter % 100 == 0) {
            checkUserInterrupt();
            if (verbose)
                Rprintf("   ...iteration %d\n", liter);
        }
        if (parallel) {
            estimate();
        } else {
            for (int m = 0; m < M; m++) {
                estimate(m);
            }
        }

    }

    if (verbose)
        Rprintf("   ...computing theta and phi\n");
    //compute_theta();
    //compute_phi();
    liter--;
    if (verbose)
        Rprintf("   ...complete\n");
}

void LDA::estimate() {
    tbb::parallel_for(tbb::blocked_range<int>(0, M), [&](tbb::blocked_range<int> r) {
      //Rcout << "Range " << r.begin() << " " << r.end() << "\n";
      for (int m = r.begin(); m < r.end(); ++m) {

            if (texts[m].size() == 0) return;
            arma::mat nw_tp = arma::mat(size(nw));
            arma::mat nd_tp = arma::mat(size(nd));
            arma::colvec nwsum_tp = arma::colvec(size(nwsum));
            for (int n = 0; n < texts[m].size(); n++) {
                topics[m][n] = sample(m, n, texts[m][n], nw_tp, nd_tp, nwsum_tp);
            }
            // arma::sp_mat::const_col_iterator it = data.begin_col(m);
            // arma::sp_mat::const_col_iterator it_end = data.end_col(m);
            // for(; it != it_end; ++it) {
            //     int w = it.row();
            //     nw.col(w) += nw_tp.col(w);
            // }
            // nw.col(m) += nw_tp.col(m);
            // nwsum += nwsum_tp;
        }
    });
}

void LDA::estimate(int m) {

    if (texts[m].size() == 0) return;
    arma::mat nw_tp = arma::mat(size(nw));
    arma::mat nd_tp = arma::mat(size(nd));
    arma::colvec nwsum_tp = arma::colvec(size(nwsum));
    for (int n = 0; n < texts[m].size(); n++) {
        topics[m][n] = sample(m, n, texts[m][n], nw_tp, nd_tp, nwsum_tp);
    }
    arma::sp_mat::const_col_iterator it = data.begin_col(m);
    arma::sp_mat::const_col_iterator it_end = data.end_col(m);
    for(; it != it_end; ++it) {
        int w = it.row();
        nw.col(w) += nw_tp.col(w);
    }
    nw.col(m) += nw_tp.col(m);
    nwsum += nwsum_tp;

}


int LDA::sample(int m, int n, int w) {

    // remove z_i from the count variables
    int topic = topics[m][n];
    nw.at(topic, w) -= 1;
    nd.at(topic, m) -= 1;
    nwsum[topic] -= 1;

    double Vbeta = V * beta;
    double Kalpha = K * alpha;
    // do multinomial sampling via cumulative method
    for (int k = 0; k < K; k++) {
        if (fitted) {
            p[k] = (nw.at(k, w) + nw_ft.at(k, w) + beta) / (nwsum[k] + nwsum_ft[k] + Vbeta) *
                   (nd.at(k, m) + alpha) / (ndsum[m] + Kalpha);
        } else {
            p[k] = (nw.at(k, w) + beta) / (nwsum[k] + Vbeta) *
                   (nd.at(k, m) + alpha) / (ndsum[m] + Kalpha);
        }
    }

    // cumulate multinomial parameters
    for (int k = 1; k < K; k++) {
        p[k] += p[k - 1];
    }
    // scaled sample because of unnormalized p[]
    double u = random_prob(generator) * p[K - 1];

    // rejection sampling
    for (int k = 0; k < K; k++) {
        topic = k;
        if (p[k] > u) {
            break;
        }
    }

    // add newly estimated z_i to count variables
    nw.at(topic, w) += 1;
    nd.at(topic, m) += 1;
    nwsum[topic] += 1;

    return topic;
}

int LDA::sample(int m, int n, int w,
                arma::mat& nw_tp, arma::mat& nd_tp, arma::colvec& nwsum_tp) {

    // remove z_i from the count variables
    int topic = topics[m][n];
    nw_tp.at(topic, w) -= 1;
    nd_tp.at(topic, m) -= 1;
    nwsum_tp[topic] -= 1;

    double Vbeta = V * beta;
    double Kalpha = K * alpha;
    // do multinomial sampling via cumulative method
    for (int k = 0; k < K; k++) {
        if (fitted) {
            p[k] = (nw.at(k, w) + nw_ft.at(k, w) + beta) / (nwsum[k] + nwsum_ft[k] + Vbeta) *
                (nd.at(k, m) + alpha) / (ndsum[m] + Kalpha);
        } else {
            p[k] = (nw.at(k, w) + nw_tp.at(k, w) + beta) / (nwsum[k] + nwsum_tp[k] + Vbeta) *
                (nd.at(k, m) + nd_tp.at(k, m) + alpha) / (ndsum[m] + Kalpha);
        }
    }

    // cumulate multinomial parameters
    for (int k = 1; k < K; k++) {
        p[k] += p[k - 1];
    }
    // scaled sample because of unnormalized p[]
    double u = random_prob(generator) * p[K - 1];

    // rejection sampling
    for (int k = 0; k < K; k++) {
        topic = k;
        if (p[k] > u) {
            break;
        }
    }

    // add newly estimated z_i to count variables
    nw_tp.at(topic, w) += 1;
    nd_tp.at(topic, m) += 1;
    nwsum_tp[topic] += 1;

    return topic;
}

void LDA::compute_theta() {
    for (int m = 0; m < M; m++) {
        for (int k = 0; k < K; k++) {
            theta.at(m, k) = (nd.at(k, m) + alpha) / (ndsum[m] + K * alpha);
        }
    }
}

void LDA::compute_phi() {
    for (int k = 0; k < K; k++) {
        for (int w = 0; w < V; w++) {
            phi.at(k, w) = (nw.at(k, w) + nw_ft.at(k, w) + beta) / (nwsum[k] + nwsum_ft[k] + V * beta);
        }
    }
}
