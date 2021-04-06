context("internal functions")

require(quanteda)
dict <- dictionary(list(A = c("a", "aa*", "abc"),
                        B = c("b*", "bb*", "bcd"),
                        AB = c("aa", "bb")))
txt <- c("a aa aa aaa abc", "b b bb bcd bcd")
dfmt <- dfm(tokens(txt))

test_that("tfm is working", {

    tfm1 <- seededlda:::tfm(dfmt, dict)
    expect_equal(tfm1["A",], c("a" = 0.1, "aa" = 0.1, "aaa" = 0.1, "abc" = 0.1,
                               "b" = 0, "bb" = 0, "bcd" = 0))
    expect_equal(tfm1["B",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                               "b" = 0.1, "bb" = 0.1, "bcd" = 0.1))
    expect_equal(tfm1["AB",], c("a" = 0, "aa" = 0.1, "aaa" = 0, "abc" = 0,
                                "b" = 0, "bb" = 0.1, "bcd" = 0))
    expect_equal(tfm1["other",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                                   "b" = 0, "bb" = 0, "bcd" = 0))

    tfm2 <- seededlda:::tfm(dfmt, dict, residual = FALSE)
    expect_equal(rownames(tfm2), c("A", "B", "AB"))

    tfm3 <- seededlda:::tfm(dfmt, dict, min_termfreq = 2)
    expect_equal(tfm3["A",], c("a" = 0, "aa" = 0.1, "aaa" = 0, "abc" = 0,
                               "b" = 0, "bb" = 0, "bcd" = 0))
    expect_equal(tfm3["B",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                               "b" = 0.1, "bb" = 0, "bcd" = 0.1))
    expect_equal(tfm3["AB",], c("a" = 0, "aa" = 0.1, "aaa" = 0, "abc" = 0,
                                "b" = 0, "bb" = 0, "bcd" = 0))
    expect_equal(tfm3["other",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                                   "b" = 0, "bb" = 0, "bcd" = 0))
})
