require(seededlda)
require(quanteda)

#data_corpus_guardian <- readRDS('/home/kohei/Dropbox/Public/data_corpus_guardian2016-10k.rds')
data_corpus_guardian <- readRDS('C:/Users/watan/Dropbox/Public/data_corpus_guardian2016-10k.rds')

toks <- tokens(data_corpus_guardian,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_number = TRUE)
dfmt <- dfm(toks) %>%
    dfm_remove(stopwords(), min_nchar = 2) %>%
    dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")

quanteda_options(threads = 1)

options(seededlda_batch_size = 1000)
options(seededlda_threads = 6)
lda1 <- textmodel_lda(dfmt, k = 20, verbose = TRUE, max_iter = 200)

options(seededlda_batch_size = NULL)
lda2 <- textmodel_lda(dfmt, k = 20, verbose = TRUE, max_iter = 200)

microbenchmark::microbenchmark(
    para = {options(seededlda_batch_size = 2000)
            textmodel_lda(dfmt, k = 20, verbose = TRUE, max_iter = 1000)},
    seri = {options(seededlda_batch_size = NULL)
            textmodel_lda(dfmt, k = 20, verbose = TRUE, max_iter = 1000)},
    times = 10
)

terms(lda)
table(topics(lda))
