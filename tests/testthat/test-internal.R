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

    tfm4 <- seededlda:::tfm(dfmt, dict, min_termfreq = 2, residual = 4)
    expect_equal(rownames(tfm4),
                 c("A", "B", "AB", "other1", "other2", "other3", "other4"))

    options(slda_residual_name = "topic")
    tfm5 <- seededlda:::tfm(dfmt, dict, min_termfreq = 2, residual = 4)
    expect_equal(rownames(tfm5),
                 c("A", "B", "AB", "topic1", "topic2", "topic3", "topic4"))
    options(slda_residual_name = "other") # reset

    tfm6 <- seededlda:::tfm(dfmt, dict, weight_scheme = "topic", residual = 0)
    expect_equal(tfm6["A",], c("a" = 0.05, "aa" = 0.05, "aaa" = 0.05, "abc" = 0.05,
                               "b" = 0, "bb" = 0, "bcd" = 0))
    expect_equal(tfm6["B",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                               "b" = 0.05, "bb" = 0.05, "bcd" = 0.05))
    expect_equal(tfm6["AB",], c("a" = 0, "aa" = 0.03, "aaa" = 0, "abc" = 0,
                                "b" = 0, "bb" = 0.03, "bcd" = 0))

    tfm7 <- seededlda:::tfm(dfmt, dict, weight_scheme = "word", residual = 0)
    expect_equal(tfm7["A",], c("a" = 0.01, "aa" = 0.02, "aaa" = 0.01, "abc" = 0.01,
                               "b" = 0, "bb" = 0, "bcd" = 0))
    expect_equal(tfm7["B",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                               "b" = 0.02, "bb" = 0.01, "bcd" = 0.02))
    expect_equal(tfm7["AB",], c("a" = 0, "aa" = 0.02, "aaa" = 0, "abc" = 0,
                                "b" = 0, "bb" = 0.01, "bcd" = 0))

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

