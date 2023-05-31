require(quanteda)

corp <- corpus_reshape(data_corpus_moviereviews[1:100])
toks <- tokens(corp,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_number = TRUE)
dfmt <- dfm(toks) %>%
    dfm_remove(stopwords(), min_nchar = 2) %>%
    dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")


test_that("gamma is working", {


    # make docid all unique
    dfmt2 <- dfmt
    dfmt2@docvars$docid_ <- dfmt2@docvars$docname_

    set.seed(1234)
    lda1 <- textmodel_lda(dfmt, k = 5, gamma = 0)

    set.seed(1234)
    expect_warning(
        lda2 <- textmodel_lda(dfmt2, k = 5, gamma = 0.5),
        "gamma has no effect when docid are all unique"
    )

    expect_equal(lda1$phi, lda2$phi)
    expect_equal(lda1$theta, lda2$theta)

    set.seed(1234)
    lda3 <- textmodel_lda(dfmt, k = 5, gamma = 0.1)
    expect_gt(mean(diff(as.integer(topics(lda3))) == 0, na.rm = TRUE),
              mean(diff(as.integer(topics(lda2))) == 0, na.rm = TRUE))

    set.seed(1234)
    lda4 <- textmodel_lda(dfmt, k = 5, gamma = 0.2)
    expect_gt(mean(diff(as.integer(topics(lda4))) == 0, na.rm = TRUE),
              mean(diff(as.integer(topics(lda3))) == 0, na.rm = TRUE))

    expect_error(
        textmodel_lda(dfmt, k = 5, gamma = -0.1),
        "The value of gamma must be between 0 and 1"
    )

    expect_error(
        textmodel_lda(dfmt, k = 5, gamma = 2.0),
        "The value of gamma must be between 0 and 1"
    )

    set.seed(1234)
    lda1 <- textmodel_lda(dfmt, k = 5, gamma = 0.5, max_iter = 500)
    set.seed(1234)
    lda2 <- textmodel_seqlda(dfmt, k = 5, max_iter = 500)
})

test_that("shortcut function works", {

    lda <- textmodel_seqlda(dfmt, k = 5, max_iter = 500)
    expect_equal(lda1$gamma, 0.5)
    expect_equal(lda1$max_iter, 500)

})

