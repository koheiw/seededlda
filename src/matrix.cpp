#define ARMA_NO_DEBUG
#include "lib.h"
#include "dev.h"
using namespace quanteda;
using namespace arma;
using namespace std;
using namespace Rcpp;


// https://stackoverflow.com/questions/41395120/c-custom-matrix-struct
template <typename T>
class CMatrix {

    int row, col;
    typedef tbb::concurrent_vector<T> Row;
    tbb::concurrent_vector<Row> data;

    public:
        CMatrix(int r, int c): row(r), col(c), data(r, tbb::concurrent_vector<T>(c, 0)) {}

        // allow to use matrix[i][j]
        Row & operator[](int i) {
            return data[i];
        }
};

// https://stackoverflow.com/questions/41395120/c-custom-matrix-struct
template <typename T>
class SMatrix {

    int row, col;
    typedef std::vector<T> Row;
    std::vector<Row> data;

    public:
        SMatrix(int r, int c): row(r), col(c), data(r, std::vector<T>(c, 0)) {}

        // allow to use matrix[i][j]
        Row & operator[](int i) {
            return data[i];
        }
};


// [[Rcpp::export]]
void test_cmat(int nrow, int ncol) {
    CMatrix <int> cmat(nrow, ncol);
    for (int i = 0; i < nrow; i++) {
        for (int j = 0; j < ncol; j++) {
            cmat[i][j] += 1;
        }
    }
    //Rcout << cmat[nrow - 1][ncol - 1] << "\n";
}

// [[Rcpp::export]]
void test_smat(int nrow, int ncol) {
    SMatrix <int> smat(nrow, ncol);
    for (int i = 0; i < nrow; i++) {
        for (int j = 0; j < ncol; j++) {
            smat[i][j] += 1;
        }
    }
    //Rcout << cmat[nrow - 1][ncol - 1] << "\n";
}


// [[Rcpp::export]]
void test_arma(int nrow, int ncol) {
    arma::mat amat(nrow, ncol, arma::fill::zeros);
    for (int i = 0; i < nrow; i++) {
        for (int j = 0; j < ncol; j++) {
            amat.at(i, j) += 1;
        }
    }
    //Rcout << amat.at(nrow - 1, ncol - 1) << "\n";
}

// [[Rcpp::export]]
arma::mat test_reshae(int col, int row) {
    std::vector<int> temp = {1, 2, 3, 4, 5, 6};
    arma::vec mt = arma::conv_to<arma::vec>::from(temp);
    mt.reshape(col, row);
    mt = mt.t();
    //mt = resize(mt, col, row);
    //Rcout << mt;
    //mt = mt.t();
    return mt;
}

/***R
test_reshae(2, 3)
# microbenchmark::microbenchmark(
#     test_cmat(1000, 1000),
#     test_smat(1000, 1000),
#     test_arma(1000, 1000),
#     test_cmat(1000, 10000),
#     test_smat(1000, 10000),
#     test_arma(1000, 10000),
#     test_cmat(1000, 100000),
#     test_smat(1000, 100000),
#     test_arma(1000, 100000)
# )

***/
