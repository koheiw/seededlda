#include "array.h"
#include <testthat.h>

context("Array") {

    arma::mat mt = {{1, 2}, {3, 4}, {5, 6}};
    arma::colvec cv = {1, 2, 3, 4};
    arma::rowvec rv = {1, 2, 3, 4};

    Array arr_mt(mt);
    Array arr_cv(cv);
    Array arr_rv(rv);

    test_that("constructors are working") {
        expect_true(arr_mt.row == 2);
        expect_true(arr_mt.col == 3);
        expect_true(arr_mt.data.size() == 2);
        expect_true(arr_mt.data[0].size() == 3);

        expect_true(arr_cv.row == 1);
        expect_true(arr_cv.col == 4);
        expect_true(arr_cv.data.size() == 1);
        expect_true(arr_cv.data[0].size() == 4);

        expect_true(arr_rv.row == 1);
        expect_true(arr_rv.col == 4);
        expect_true(arr_rv.data.size() == 1);
        expect_true(arr_rv.data[0].size() == 4);
    }

    test_that("accessors are working") {
        expect_true(arr_mt.at(0, 1) == 1);
        expect_true(arr_mt.at(1, 2) == 6);

        expect_true(arr_cv.at(1) == 1);
        expect_true(arr_cv.at(4) == 4);

        expect_true(arr_rv.at(1) == 1);
        expect_true(arr_rv.at(4) == 4);
    }

    test_that("operators are working") {
        Array arr_mt0(2, 3);
        arr_mt0 += arr_mt;
        expect_true(arr_mt0[0] == arr_mt[0]);
        expect_true(arr_mt0[1] == arr_mt[1]);

        Array arr_cv0(1, 4);
        arr_cv0 += arr_cv;
        expect_true(arr_cv0[0] == arr_cv[0]);

        Array arr_rv0(1, 4);
        arr_rv0 += arr_rv;
        expect_true(arr_rv0[0] == arr_rv[0]);
    }
}
