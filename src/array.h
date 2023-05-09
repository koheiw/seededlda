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
    Array(arma::mat mt): row(mt.n_rows), col(mt.n_cols), data(to_data(mt)) {}
    Array(arma::sp_mat smt): row(smt.n_rows), col(smt.n_cols), data(to_data(smt)) {}

    // allow access by .at()
    int & at(int i, int j) {
        // if (i < 0 || row <= i)
        //     throw std::range_error("Invalid row index");
        // if (j < 0 || row <= j)
        //     throw std::range_error("Invalid column index");
        return data[i][j];
    }
    int & at(int j) {
        //if (j < 0 || col <= j)
        //    throw std::range_error("Invalid index");
        return data[0][j];
    }

    // allow access by [i][j]
    Row & operator[](std::size_t i) {
        return data[i];
    }

    // allow addition by +=
    Array & operator+=(const Array &arr) {
        if (row != arr.row || col != arr.col)
            throw std::invalid_argument("The sizes of objects do not match");
        //if (row != arr.data.size())
        //    throw std::range_error("Invalid number of rows");
        for (std::size_t i = 0; i < data.size(); i++) {
            //if (col != arr.data[i].size())
            //    throw std::range_error("Invalid number of columns");
            for (std::size_t j = 0; j < data[i].size(); j++) {
                data[i][j] += arr.data[i][j];
            }
        }
        return *this;
    }

    //convert to arma::mat
    arma::mat to_mat() {
        std::vector<int> temp;
        temp.reserve(row * col);
        for (std::size_t i = 0;  i < data.size(); i++) {
            temp.insert(temp.end(), data[i].begin(), data[i].end());
        }
        arma::vec mt = arma::conv_to<arma::vec>::from(temp);
        mt.reshape(col, row);
        mt = mt.t();
        return mt;
    }

    //convert to arma::sp_mat
    arma::sp_mat to_smat() {
        arma::mat mt = to_mat();
        return arma::sp_mat(mt);
    }

    private:

    Data to_data(arma::mat &mt) {
        Data temp(mt.n_rows, std::vector<int>(mt.n_cols, 0));
        for (std::size_t i = 0;  i < mt.n_rows; i++) {
            for (std::size_t j = 0;  j < mt.n_cols; j++) {
                temp[i][j] = mt.at(i, j);
            }
        }
        return temp;
    }
    Data to_data(arma::sp_mat &smt) {
        arma::mat mt(smt);
        return to_data(mt);
    }
};
