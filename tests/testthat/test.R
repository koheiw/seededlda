context("test")

require(quanteda)
toks <- tokens(data_corpus_irishbudget2010)
dfmat <- dfm(toks)
dict <- dictionary(file = "../data/topics.yml")

test_that("test tfm", {
    mat1 <- quanteda.seededlda:::tfm(dfmat, dict, residual = FALSE)
    expect_equal(dim(mat1), c(length(dict), nfeat(dfmat)))
    mat2 <- quanteda.seededlda:::tfm(dfmat, dict, residual = TRUE)
    expect_equal(dim(mat2), c(length(dict) + 1, nfeat(dfmat)))

    feat <- colSums(mat1)
    feat <- feat[feat > 0]
    expect_true(setequal(featnames(dfm_select(dfmat, dict)), names(feat)))
})

test_that("test tfm with extreme cases", {
    dict1 <- dictionary(list(economy = "market*", politics = "parliament*", something = "xyz"))
    expect_equal(dim(quanteda.seededlda:::tfm(dfmat, dict1, residual = FALSE)),
                 c(3, 5140))

    dict2 <- dictionary(list(economy = "market*", politics = "parliament*", something = ""))
    expect_equal(dim(quanteda.seededlda:::tfm(dfmat, dict2, residual = FALSE)),
                 c(3, 5140))

    dict3 <- dictionary(list(economy = "", politics = "", something = ""))
    expect_equal(dim(quanteda.seededlda:::tfm(dfmat, dict3, residual = FALSE)),
                 c(3, 5140))
})
