require(seededlda)
require(quanteda)

dict <- dictionary(file = "tests/data/topics.yml")
#corp <- readRDS('/home/kohei/Dropbox/Public/data_corpus_guardian2016-10k.rds')
corp <- readRDS('C:/Users/watan/Dropbox/Public/data_corpus_guardian2016-10k.rds')
corp <- corpus_reshape(corp)

toks <- tokens(corp,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_number = TRUE)
dfmt <- dfm(toks) %>%
    dfm_remove(stopwords(), min_nchar = 2) %>%
    dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")

lda0 <- textmodel_seededlda(dfmt, dict, verbose = TRUE, max_iter = 200,
                            batch_size = 1.0, gamma = 0.5)
lda1 <- textmodel_seededlda(dfmt, dict, verbose = TRUE, max_iter = 200,
                            batch_size = 0.1, gamma = 0.5)
lda2 <- textmodel_seededlda(dfmt, dict, verbose = TRUE, max_iter = 200,
                            batch_size = 0.01, gamma = 0.5)
lda3 <- textmodel_seededlda(dfmt, dict, verbose = TRUE, max_iter = 200,
                            batch_size = 0.01, gamma = 0.2)

sizes(lda0)
sizes(lda1)
sizes(lda2)
sizes(lda3)

matplot(lda0$theta[1:100,], type = "l", lty = 1)
matplot(lda1$theta[1:100,], type = "l", lty = 1)

microbenchmark::microbenchmark(
    para = textmodel_lda(dfmt, k = 20, verbose = TRUE, batch_size = 0.01),
    auto = textmodel_lda(dfmt, k = 20, verbose = TRUE, batch_size = 0.01, auto_iter = TRUE),
    seri = textmodel_lda(dfmt, k = 20, verbose = TRUE),
    times = 1
)

