require(quanteda)
data(data_corpus_moviereviews, package = "quanteda.textmodels")

toks <- tokens(data_corpus_moviereviews[1:500],
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_number = TRUE)

dfmt <- dfm(toks) %>%
    dfm_remove(stopwords(), min_nchar = 2) %>%
    dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")

dict <- dictionary(list(romance = c("love*", "couple*"),
                        sifi = c("alien*", "star", "space")))
set.seed(1234)
lda <- textmodel_lda(dfmt, k = 5)

set.seed(1234)
slda <- textmodel_seededlda(dfmt, dict, residual = TRUE, weight = 0.02,
                            min_termfreq = 10)

test_that("divergence() is working", {

    expect_equal(divergence(lda),
                 2.94, tolerance = 0.1)

    expect_equal(divergence(slda),
                 3.78, tolerance = 0.1)

})

test_that("sizes() is working", {
    expect_equal(sizes(lda),
                 c("topic1" = 0.15, "topic2" = 0.14, "topic3" = 0.40,
                   "topic4" = 0.14, "topic5" = 0.139),
                 tolerance = 0.01)
    expect_equal(sizes(slda),
                 c("romance" = 0.33, "sifi" = 0.42, "other" = 0.24),
                 tolerance = 0.01)
})
