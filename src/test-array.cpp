#include "array.h"
#include <testthat.h>

context("C++ Array object") {

    arma::mat mt = {{1, 2}, {3, 4}, {5, 6}};
    arma::rowvec rv = {1, 2, 3, 4};
    arma::colvec cv = {1, 2, 3, 4};

    Array arr_mt(mt);
    Array arr_rv(rv);
    Array arr_cv(cv);

    test_that("constructors are working") {
        expect_true(arr_mt.row == 3L);
        expect_true(arr_mt.col == 2L);
        expect_true(arr_mt.data.size() == 3L);
        expect_true(arr_mt.data[0].size() == 2L);

        expect_true(arr_rv.row == 1L);
        expect_true(arr_rv.col == 4L);
        expect_true(arr_rv.data.size() == 1L);
        expect_true(arr_rv.data[0].size() == 4L);

        expect_true(arr_cv.row == 4L);
        expect_true(arr_cv.col == 1L);
        expect_true(arr_cv.data.size() == 4L);
        expect_true(arr_cv.data[0].size() == 1L);
    }

    test_that("accessors are working") {
        expect_true(arr_mt.at(0, 0) == 1L);
        expect_true(arr_mt.at(2, 1) == 6L);

        expect_true(arr_rv.at(0) == 1L);
        expect_true(arr_rv.at(3) == 4L);

        expect_true(arr_cv.at(0, 0) == 1L);
        expect_true(arr_cv.at(3, 0) == 4L);
    }

    test_that("operators are working") {
        Array arr_mt0(3, 2);
        arr_mt0 += arr_mt;
        expect_true(arr_mt0[0] == arr_mt[0]);
        expect_true(arr_mt0[1] == arr_mt[1]);

        Array arr_rv0(1, 4);
        arr_rv0 += arr_rv;
        expect_true(arr_rv0[0] == arr_rv[0]);

        Array arr_cv0(4, 1);
        arr_cv0 += arr_cv;
        expect_true(arr_cv0[0] == arr_cv[0]);
        expect_true(arr_cv0[1] == arr_cv[1]);
        expect_true(arr_cv0[2] == arr_cv[2]);
        expect_true(arr_cv0[3] == arr_cv[3]);

        // different sizes
        expect_error(arr_rv0 += arr_mt0);
        expect_error(arr_cv0 += arr_mt0);
    }
}
