/*
 * Copyright (C) 2007 by
 *
 * 	Xuan-Hieu Phan
 *	hieuxuan@ecei.tohoku.ac.jp or pxhieu@gmail.com
 * 	Graduate School of Information Sciences
 * 	Tohoku University
 *
 * Copyright (C) 2020-2023 by
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

//#include "array.h"

#define ARMA_NO_DEBUG
#include <RcppArmadillo.h>
#include "lib.h"
#include "dev.h"
#include <chrono>

using namespace std;
//using namespace Rcpp;
using namespace quanteda;


// LDA model
class LDA {

    public:
    // --- model parameters and variables ---
    int M; // dataset size (i.e., number of docs)
    int V; // vocabulary size
    int K; // number of topics
    int N; // total number of words
    std::vector<double> alpha, beta; // parameters for smoothing, size K
    std::vector<double> epsilon;
    double adjust; // adjust alpha by nwsum
    double Vbeta, Kalpha; // parameters for smoothing
    int max_iter; // number of Gibbs sampling iterations
    int iter; // the current iteration of the training
    double min_delta; // criteria for convergence
    int random; // seed for random number generation
    int batch; // size of subsets to distribute
    bool verbose; // print progress messages
    int thread; // numebr of thread in parallel processing

    // topic transition
    double gamma; // parameter for topic transition
    std::vector<bool> first; // first[m], documents m are first sentences, size M

    arma::sp_mat data; // transposed document-feature matrix
    Texts texts; // individual words
    Texts z; // topic assignments for words, size M x doc.size()
    arma::mat nw; // nw[k][v]: number of instances of word/term v assigned to topic k, size K x V
    arma::mat nd; // nd[k][m]: number of words in document m assigned to topic k, size K x M
    arma::rowvec nwsum; // nwsum[k]: total number of words assigned to topic k, size K
    arma::rowvec ndsum; // ndsum[m]: total number of words in document m, size M

    // prediction with fitted model
    bool fitted;
    arma::mat nw_ft;
    arma::rowvec nwsum_ft;

    // estimated parameters
    arma::mat theta; // theta: document-topic distributions, size M x K
    arma::mat phi; // phi: topic-word distributions, size K x V

    // random number generators
    std::default_random_engine generator;
    std::uniform_real_distribution<double> random_prob;
    std::uniform_int_distribution<int> random_topic;

    // --------------------------------------

    // constructor
    LDA(int k, std::vector<double> alpha, std::vector<double> beta, double gamma,
        int max_iter, double min_delta, double adjust,
        int random, int batch, bool verbose, int thread);

    // set default values for variables
    void set_default_values(int k);
    void set_data(arma::sp_mat mt, std::vector<bool> first);
    void set_fitted(arma::sp_mat mt);

    // init for estimation
    int initialize();

    // estimate LDA model using Gibbs sampling
    void estimate();
    int sample(int m, int n, int w, std::vector<double> &prob, arma::mat &nw__, arma::rowvec &nwsum__);
    void compute_theta();
    void compute_phi();

};

LDA::LDA(int k, std::vector<double> alpha, std::vector<double> beta, double gamma, int max_iter,
         double min_delta, double adjust, int random, int batch, bool verbose, int thread) {

	set_default_values(k);

    if (verbose)
        Rprintf("Fitting LDA with %d topics\n", K);

    if (K == (int)alpha.size()) {
    	this->alpha = alpha;
    } else {
    	throw std::invalid_argument("Invalid alpha");
    }
    this->adjust = adjust;

    if (K == (int)beta.size()) {
    	this->beta = beta;
    } else {
    	throw std::invalid_argument("Invalid beta");
    }

    if (0 < gamma)
        this->gamma = gamma;
    if (0 < max_iter)
        this->max_iter = max_iter;
    if (0 < thread)
        this->thread = thread;
    this->min_delta = min_delta;
    this->random = random;
    this->batch = batch;
    this->verbose = verbose;

}

void LDA::set_default_values(int k) {

	K = k;
    M = 0;
    V = 0;
    N = 0;
    alpha = std::vector<double>(K, 0.5);
    beta = std::vector<double>(K, 0.1);
    epsilon = std::vector<double>(K, 0.0);
    adjust = 0;
    max_iter = 2000;
    iter = 0;
    verbose = false;
    min_delta = -1.0;
    random = 1234;
    gamma = 0;
    first = std::vector<bool>(M);
    thread = -1;
    fitted = false;

}

void LDA::set_data(arma::sp_mat mt, std::vector<bool> first) {

    data = mt.t();
    M = data.n_cols;
    V = data.n_rows;
    N = arma::accu(data);
    this->first = first;

    //printf("M = %d, V = %d\n", M, V);
}

void LDA::set_fitted(arma::sp_mat words) {

    if ((int)words.n_rows != V || (int)words.n_cols != K) {
        throw std::invalid_argument("Invalid word matrix");
    }
	if (arma::accu(words) > 0) {
	    if (verbose)
	        Rprintf(" ...loading fitted model\n");
	    nw_ft = arma::mat(words.t());
	    nwsum_ft = arma::sum(nw_ft, 1);
	    fitted = true;
	}
}

int LDA::initialize() {

    if (verbose)
        Rprintf(" ...initializing\n");

    std::default_random_engine generator(random);
    std::uniform_real_distribution< double > random_prob(0, 1);
    std::uniform_int_distribution< int > random_topic(0, K - 1);

    theta = arma::mat(M, K, arma::fill::zeros);
    phi = arma::mat(K, V, arma::fill::zeros);

    nw = arma::mat(K, V);
    nd = arma::mat(K, M);
    nwsum = arma::rowvec(K);
    ndsum = arma::rowvec(arma::sum(data, 0));

	Kalpha = 0;
	for (auto& a : alpha)
		Kalpha += a;
	Vbeta = 0;
	for (auto& b : beta)
		Vbeta += V * b / K;

    // initialize z and texts
    z = Texts(M);
    texts = Texts(M);
    for (int m = 0; m < M; m++) {
        z[m] = Text(ndsum(m));
        texts[m] = Text(ndsum(m));
        arma::sp_mat::const_col_iterator it = data.begin_col(m);
        arma::sp_mat::const_col_iterator it_end = data.end_col(m);
        int i = 0;
        for(; it != it_end; ++it) {
            int w = it.row();
            int F = *it;
            for (int f = 0; f < F; f++) {
                texts[m][i] = w;
                i++;
            }
        }
    }
    //dev::Timer timer;
    //dev::start_timer("Set z", timer);
    for (int m = 0; m < M; ++m) {
        if (texts[m].empty()) continue;
        for (std::size_t i = 0; i < texts[m].size(); i++) {
            int topic = random_topic(generator);
            int w = texts[m][i];
            z[m][i] = topic;
            // number of words in document m assigned to topic j
            nd(topic, m) += 1;
            // number of instances of word w assigned to topic j
            nw(topic, w) += 1;
            // total number of words assigned to topic j
            nwsum(topic) += 1;
        }
    }
    //dev::stop_timer("Set z", timer);

    // compute epsilon by the size of topics
	if (adjust > 0) {
	    for (int k = 0; k < K; k++) {
	    	epsilon[k] = (alpha[k] / nwsum(k)) * adjust;
	    }
	}
    return 0;
}

void LDA::estimate() {

    if (verbose && thread > 1 && batch != M) {
        Rprintf(" ...using up to %d threads for distributed computing\n", thread);
        Rprintf(" ......allocating %d documents to each thread\n", batch);
    }
    if (verbose) {
        if (min_delta == -1) {
            Rprintf(" ...Gibbs sampling in %d iterations\n", max_iter);
        } else {
            Rprintf(" ...Gibbs sampling in up to %d iterations\n", max_iter);
        }
    }

    int change, change_pv = 0;
    auto start = std::chrono::high_resolution_clock::now();
    int iter_inc = 10;
    std::mutex mutex_sync;
    while (true) {

        checkUserInterrupt();
        if (verbose && iter > 0 && iter % 100 == 0)
            Rprintf(" ......iteration %d", iter);

        change = 0;
#if QUANTEDA_USE_TBB
        tbb::task_arena arena(thread);
        arena.execute([&]{
            tbb::parallel_for(tbb::blocked_range<int>(0, M, batch), [&](tbb::blocked_range<int> r) {
                int begin = r.begin();
                int end = r.end();
#else
                int begin = 0;
                int end = M;
#endif
                // partitions must match first documents when gamma > 0
                if (gamma > 0) {
                    while (begin != 0 && !first[begin]) begin--;
                    while (end != M && !first[end]) end--;
                }

				// local topic assignment
                arma::mat nw__(K, V);
                arma::rowvec nwsum__(K);
                int change__ = 0;
                for (int i = 0; i < iter_inc; i++) {
                    //for (int m = r.begin(); m < r.end(); ++m) {
                    for (int m = begin; m < end; ++m) {
                        // topic of the previous document
                        std::vector<double> prob(K);
                        for (int k = 0; k < K; k++) {
                            if (gamma == 0 || first[m] || m == 0) {
                                prob[k] = 1.0 / K;
                            } else {
                                prob[k] = pow((nd(k, m - 1) + alpha[k]) /
                                	         (ndsum(m - 1) + K * alpha[k]), gamma);
                            }
                        }
                        if (texts[m].empty()) continue;
                        for (std::size_t n = 0; n < texts[m].size(); n++) {
                            int w = texts[m][n];
                            unsigned int topic = sample(m, n, w, prob, nw__, nwsum__);
                            if (z[m][n] != topic) {
                                change__++;
                                z[m][n] = topic;
                            }
                        }
                    }
                }
                mutex_sync.lock();
                change += change__;
                nw += nw__;
                nwsum += nwsum__;
                // adjust alpha by the changes in sizes
                if (adjust > 0) {
	                for (int k = 0; k < K; k++) {
	                	alpha[k] = std::max(0.0, alpha[k] + (epsilon[k] * nwsum__(k)));
                	}
                }
                mutex_sync.unlock();
#if QUANTEDA_USE_TBB
            }, tbb::static_partitioner());
        });
#endif
        if (iter > 0 && iter % 100 == 0) {
            double delta = (double)(change_pv - change) / (double)(iter_inc * N);
            if (verbose) {
                auto end = std::chrono::high_resolution_clock::now();
                auto diff = std::chrono::duration<double, std::milli>(end - start);
                double msec = diff.count();
                Rprintf(" elapsed time: %.2f seconds (delta: %.2f%%)\n", msec / 1000, delta * 100);
            }
            if (min_delta > delta)
                break;
        }
        if (iter >= max_iter)
            break;
        change_pv = change;
        iter += iter_inc;
    }
    if (verbose)
        Rprintf(" ...computing theta and phi\n");
    if (verbose)
        Rprintf(" ...complete\n");
}

int LDA::sample(int m, int n, int w,
                std::vector<double> &prob,
                arma::mat &nw__, arma::rowvec &nwsum__) {

    // remove z_i from the count variables
    int topic = z[m][n];
    //Rcout << "topic:" << topic << "\n";
    nw__(topic, w) -= 1;
    nwsum__(topic) -= 1;
    nd(topic, m) -= 1;
    std::vector<double> p(K, 0);

    // do multinomial sampling via cumulative method
    for (int k = 0; k < K; k++) {
    	if (fitted) {
    		p[k] = ((nw(k, w) + nw__(k, w) + nw_ft(k, w) + beta[k]) /
    			    (nwsum(k) + nwsum__(k) + nwsum_ft(k) + Vbeta)) *
    			   ((nd(k, m) + alpha[k]) /
    			    (ndsum(m) + Kalpha)) * prob[k];
    	} else {
    		p[k] = ((nw(k, w) + nw__(k, w) + beta[k]) /
    			    (nwsum(k) + nwsum__(k) + Vbeta)) *
    			   ((nd(k, m) + alpha[k]) /
    				(ndsum(m) + Kalpha)) * prob[k];
    	}
    }
    // cumulate multinomial parameters
    for (int k = 1; k < K; k++) {
        p[k] += p[k - 1];
    }
    // scaled sample because of unnormalized p[]
    double u = random_prob(generator) * p[K - 1];

    // inverse-CDF sampling
    for (int k = 0; k < K; k++) {
        topic = k;
        if (p[k] > u) {
            break;
        }
    }

    // add newly estimated z_i to count variables
    nw__(topic, w) += 1;
    nwsum__(topic) += 1;
    nd(topic, m) += 1;
    return topic;
}

void LDA::compute_theta() {

	if (adjust > 0) {
		// update for floating point errors
		Kalpha = 0;
		for (auto& a : alpha)
			Kalpha += a;
	}

    for (int m = 0; m < M; m++) {
        for (int k = 0; k < K; k++) {
            theta(m, k) = (nd(k, m) + alpha[k]) / (ndsum(m) + Kalpha);
        }
    }
}

void LDA::compute_phi() {

	// Vbeta = 0;
	// for (auto& b : beta)
	//	Vbeta += V * b / K;

    for (int k = 0; k < K; k++) {
        for (int w = 0; w < V; w++) {
        	if (fitted) {
            	phi(k, w) = (nw(k, w) + nw_ft(k, w) + beta[k]) / (nwsum(k) + nwsum_ft(k) + Vbeta);
        	} else {
        		phi(k, w) = (nw(k, w) + beta[k]) / (nwsum(k) + Vbeta);
        	}
        }
    }
}
