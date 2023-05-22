require(seededlda)
require(quanteda)

data_corpus_guardian <- readRDS('../Dropbox/Public/data_corpus_guardian2016.rds')
data_corpus_guardian <- readRDS('/home/kohei/Dropbox/Public/data_corpus_guardian2016-10k.rds')
#data_corpus_guardian <- readRDS('C:/Users/watan/Dropbox/Public/data_corpus_guardian2016-10k.rds')

toks <- tokens(data_corpus_guardian,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_number = TRUE)
dfmt <- dfm(toks) %>%
    dfm_remove(stopwords(), min_nchar = 2) %>%
    dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")

lda0 <- textmodel_lda(dfmt, k = 20, verbose = TRUE, max_iter = 200, batch_size = 1)
lda1 <- textmodel_lda(dfmt, k = 20, verbose = TRUE, max_iter = 200, batch_size = 0.5)
lda2 <- textmodel_lda(dfmt, k = 20, verbose = TRUE, max_iter = 200, batch_size = 0.2)
lda3 <- textmodel_lda(dfmt, k = 20, verbose = TRUE, max_iter = 200, batch_size = 0.1)
lda4 <- textmodel_lda(dfmt, k = 20, verbose = TRUE, max_iter = 200, batch_size = 0.05)

microbenchmark::microbenchmark(
    para = textmodel_lda(dfmt, k = 20, verbose = TRUE, batch_size = 0.01),
    auto = textmodel_lda(dfmt, k = 20, verbose = TRUE, batch_size = 0.01, auto_iter = TRUE),
    seri = textmodel_lda(dfmt, k = 20, verbose = TRUE),
    times = 1
)

system.time(textmodel_lda(dfmt, k = 20, verbose = TRUE, batch_size = 0.01))
system.time(textmodel_lda(dfmt, k = 20, verbose = TRUE, batch_size = 0.01, auto_iter = TRUE))
system.time(textmodel_lda(dfmt, k = 20, verbose = TRUE))
