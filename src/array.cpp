#define ARMA_NO_DEBUG
#include "lib.h"
#include "dev.h"
using namespace quanteda;
using namespace arma;
using namespace std;
using namespace Rcpp;

// Matrix-like object
class Array {

    public:
        std::size_t row, col;
        typedef std::vector<int> Row;
        typedef std::vector<Row> Data;
        Data data;
        Array(std::size_t r, std::size_t c): row(r), col(c), data(r, std::vector<int>(c, 0)) {}

        // allow access by [i][j]
        Row & operator[](std::size_t i) {
            return data[i];
        }

        // allow additon by +=
        Array& operator+=(const Array &arr) {
            if (this->data.size() != arr.data.size())
                throw std::range_error("Invalid number of rows");
            for (std::size_t i = 0; i < this->data.size(); i++) {
                if (this->data[i].size() != arr.data[i].size())
                    throw std::range_error("Invalid number of colmuns");
                for (std::size_t j = 0; j < this->data[i].size(); j++) {
                    this->data[i][j] += arr.data[i][j];
                }
            }
            return *this;
        }
};


// [[Rcpp::export]]
void test_array(std::size_t nrow, std::size_t ncol) {
    Array arr1(nrow, ncol);
    for (std::size_t i = 0; i < nrow; i++) {
        for (std::size_t j = 0; j < ncol; j++) {
            arr1[i][j] = 2;
        }
    }
    Array arr2(nrow, ncol);
    for (std::size_t i = 0; i < nrow; i++) {
        for (std::size_t j = 0; j < ncol; j++) {
            arr2[i][j] = 3;
        }
    }
    arr1 += arr2;
    // for (std::size_t i = 0; i < nrow; i++) {
    //     for (std::size_t j = 0; j < ncol; j++) {
    //         Rcout << i << " " << j << ": " << arr1[i][j] << "\n";
    //     }
    // }
}

// [[Rcpp::export]]
void test_arma(std::size_t nrow, std::size_t ncol) {
    arma::mat mat1(nrow, ncol);
    mat1.fill(2);
    arma::mat mat2(nrow, ncol);
    mat1.fill(3);
    mat1 += mat2;
}

/***R
microbenchmark::microbenchmark(
    test_array(1000, 1000),
    test_arma(1000, 1000),
    test_array(1000, 10000),
    test_arma(1000, 10000)
)
***/
