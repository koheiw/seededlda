#include "lib.h"
#include "dev.h"
#include <chrono>

using namespace std;

// Matrix-like object
class Array {

public:
    std::size_t row, col;
    typedef std::vector<int> Row;
    typedef std::vector<Row> Data;
    Data data;

    // constructors
    Array(): row(0), col(0), data(0, std::vector<int>(0, 0)) {} // empty
    Array(std::size_t n): row(1), col(n), data(1, std::vector<int>(n, 0)) {} // vector
    Array(std::size_t r, std::size_t c): row(r), col(c), data(r, std::vector<int>(c, 0)) {} // matrix
    Array(arma::mat x): row(x.n_rows), col(x.n_cols), data(from_mat(x)) {}

    // allow access by .at()
    int & at(int i, int j) {
        // if (this->row <= i)
        //     throw std::range_error("Invalid row index");
        // if (this->row <= j)
        //     throw std::range_error("Invalid column index");
        return data[i][j];
    }
    int & at(int j) {
        //if (this->col <= j)
        //    throw std::range_error("Invalid index");
        return data[0][j];
    }

    // allow access by [i][j]
    Row & operator[](std::size_t i) {
        return data[i];
    }

    // allow addition by +=
    Array& operator+=(const Array &arr) {
        if (this->row != arr.data.size())
            throw std::range_error("Invalid number of rows");
        for (std::size_t i = 0; i < this->data.size(); i++) {
            //if (this->col != arr.data[i].size())
            //    throw std::range_error("Invalid number of colmuns");
            for (std::size_t j = 0; j < this->data[i].size(); j++) {
                this->data[i][j] += arr.data[i][j];
            }
        }
        return *this;
    }

    // convert from arma::mat
    Data from_mat(arma::mat &mt) {
        Data temp(mt.n_rows, std::vector<int>(mt.n_cols, 0));
        for (std::size_t i = 0;  i < mt.n_rows; i++) {
            for (std::size_t j = 0;  j < mt.n_cols; j++) {
                temp[i][j] = mt.at(i, j);
            }
        }
        return temp;
    }
    // convert to arma::sp_mat
    // arma::sp_mat to_spmat() {
    //     for (std::size_t i = 0;  i < x.n_rows; i++) {
    //         for (std::size_t j = 0;  j < x.n_cols; j++) {
    //             // https://arma.sourceforge.net/docs.html#SpMat
    //         }
    //     }
    // }
};
