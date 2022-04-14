require(quanteda)

test_that("tfm is working", {

    dict <- dictionary(list(A = c("a", "aa*", "abc"),
                            B = c("b*", "bb*", "bcd"),
                            AB = c("aa", "bb")))
    txt <- c("a aa aa aaa abc", "b b bb bcd bcd")
    dfmt <- dfm(tokens(txt))

    tfm1 <- seededlda:::tfm(dfmt, dict)
    expect_s4_class(tfm1, "dgCMatrix")
    expect_equal(tfm1["A",], c("a" = 0.1, "aa" = 0.1, "aaa" = 0.1, "abc" = 0.1,
                               "b" = 0, "bb" = 0, "bcd" = 0))
    expect_equal(tfm1["B",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                               "b" = 0.1, "bb" = 0.1, "bcd" = 0.1))
    expect_equal(tfm1["AB",], c("a" = 0, "aa" = 0.1, "aaa" = 0, "abc" = 0,
                                "b" = 0, "bb" = 0.1, "bcd" = 0))
    expect_equal(tfm1["other",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                                   "b" = 0, "bb" = 0, "bcd" = 0))

    tfm2 <- seededlda:::tfm(dfmt, dict, residual = FALSE)
    expect_s4_class(tfm2, "dgCMatrix")
    expect_equal(rownames(tfm2), c("A", "B", "AB"))

    tfm3 <- seededlda:::tfm(dfmt, dict, min_termfreq = 2)
    expect_s4_class(tfm3, "dgCMatrix")
    expect_equal(tfm3["A",], c("a" = 0, "aa" = 0.1, "aaa" = 0, "abc" = 0,
                               "b" = 0, "bb" = 0, "bcd" = 0))
    expect_equal(tfm3["B",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                               "b" = 0.1, "bb" = 0, "bcd" = 0.1))
    expect_equal(tfm3["AB",], c("a" = 0, "aa" = 0.1, "aaa" = 0, "abc" = 0,
                                "b" = 0, "bb" = 0, "bcd" = 0))
    expect_equal(tfm3["other",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                                   "b" = 0, "bb" = 0, "bcd" = 0))

    tfm4 <- seededlda:::tfm(dfmt, dict, min_termfreq = 2, residual = 4)
    expect_s4_class(tfm4, "dgCMatrix")
    expect_equal(rownames(tfm4),
                 c("A", "B", "AB", "other1", "other2", "other3", "other4"))

    options(slda_residual_name = "topic")
    tfm5 <- seededlda:::tfm(dfmt, dict, min_termfreq = 2, residual = 4)
    expect_s4_class(tfm5, "dgCMatrix")
    expect_equal(rownames(tfm5),
                 c("A", "B", "AB", "topic1", "topic2", "topic3", "topic4"))
    options(slda_residual_name = "other") # reset

    tfm6 <- seededlda:::tfm(dfmt, dict, residual = 0, balance = TRUE)
    expect_equal(rowSums(tfm6), c("A" = 0.3, "B" = 0.3, "AB" = 0.3))

    tfm7 <- seededlda:::tfm(dfmt, dict, residual = 1, balance = TRUE)
    expect_equal(rowSums(tfm7), c("A" = 0.3, "B" = 0.3, "AB" = 0.3, "other" = 0))

    # topics without matches

    txt2 <- c("a aaa abc", "b b bcd bcd")
    dfmt2 <- dfm(tokens(txt2))

    tfm8 <- seededlda:::tfm(dfmt2, dict, residual = 0, balance = FALSE)
    expect_equal(rowSums(tfm8), c("A" = 0.21, "B" = 0.14, "AB" = 0))

    tfm9 <- seededlda:::tfm(dfmt2, dict, residual = 1, balance = FALSE)
    expect_equal(rowSums(tfm9), c("A" = 0.21, "B" = 0.14, "AB" = 0, "other" = 0))

    tfm10 <- seededlda:::tfm(dfmt2, dict, residual = 1, balance = TRUE)
    expect_equal(rowSums(tfm10), c("A" = 0.175, "B" = 0.175, "AB" = 0, "other" = 0))

})

test_that("tfm works with ngrams", {

    txt <- c("UN is the United Nations.",
             "ICC is the International Criminal Court.")
    toks <- tokens(txt)
    dict <- dictionary(list("un" = c("united nations", "un"),
                            "icc" = c("international criminal court", "icc")))
    toks1 <- tokens_compound(toks, dict, concatenator = " ")
    dfmt1 <- dfm(toks1)
    expect_equal(rowSums(seededlda:::tfm(dfmt1, dict)),
                 c("un" = 0.2, "icc" = 0.2, "other" = 0))

    toks2 <- tokens_compound(toks, dict, concatenator = "+")
    dfmt2 <- dfm(toks2)
    expect_equal(rowSums(seededlda:::tfm(dfmt2, dict)),
                 c("un" = 0.2, "icc" = 0.2, "other" = 0))
})

