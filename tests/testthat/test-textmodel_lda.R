context("textmodel_lda")

require(quanteda)
data(data_corpus_moviereviews, package = "quanteda.textmodels")

toks <- tokens(data_corpus_moviereviews[1:500],
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_number = TRUE)
dfmt <- dfm(toks) %>%
    dfm_remove(stopwords(), min_nchar = 2) %>%
    dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")
sifi <- c("space", "mars", "alien", "earth")

test_that("LDA is working", {

    set.seed(1234)
    lda <- textmodel_lda(dfmt, k = 5)

    expect_equal(dim(terms(lda, 10)), c(10, 5))
    expect_equal(dim(terms(lda, 20)), c(20, 5))
    expect_equal(
        colnames(terms(lda)),
        c("topic1", "topic2", "topic3", "topic4", "topic5")
    )
    expect_true(
        sum(apply(terms(lda), 2, function(x)  all(sifi %in% x))) == 1 # there is a sifi topic
    )
    expect_setequal(
       topics(lda),
       c("topic1", "topic2", "topic3", "topic4", "topic5")
    )
    expect_equal(
        levels(topics(lda)),
        c("topic1", "topic2", "topic3", "topic4", "topic5")
    )
    expect_equal(
        rowSums(lda$phi),
        c("topic1" = 1, "topic2" = 1, "topic3" = 1, "topic4" = 1, "topic5" = 1)
    )
    expect_equal(
        rowSums(lda$theta),
        structure(rep(1, ndoc(dfmt)), names = docnames(dfmt))
    )
    expect_equal(
        ncol(terms(textmodel_lda(dfmt, k = 1))), 1
    )
    expect_equal(
        ncol(terms(textmodel_lda(dfmt, k = 10))), 10
    )
    expect_error(
        textmodel_lda(dfmt, k = 0),
        "k must be larger than zero"
    )
    expect_output(
        print(lda),
        "Topics: 5; 500 documents; 22605 features."
    )

})


test_that("LDA works with empty documents", {

    dfmt_empty <- dfmt
    dfmt_empty[c(100, 200, 300),] <- 0
    dfmt_empty <- as.dfm(dfmt_empty)

    set.seed(1234)
    lda_empty <- textmodel_lda(dfmt_empty, k = 5)
    expect_true(
        all(is.na(topics(lda_empty)[c(100, 200, 300)]))
    )

})

