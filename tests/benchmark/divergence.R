require(seededlda)
require(quanteda)
require(keyATM)

#data_corpus_guardian <- readRDS('/home/kohei/Dropbox/Public/data_corpus_guardian2016-10k.rds')
data_corpus_guardian <- readRDS('C:/Users/watan/Dropbox/Public/data_corpus_guardian2016-10k.rds')

#dict <- dictionary(file = "tests/data/topics.yml")
toks <- tokens(data_corpus_guardian,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_number = TRUE)

dfmt <- dfm(toks) %>%
    dfm_remove(stopwords(), min_nchar = 2) %>%
    dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")

dat <- data.frame()
for (k in seq(10, 50, by = 10)) {
    cat(k, "\n")
    set.seed(1234)
    slda1 <- textmodel_lda(dfmt, k = k, max_iter = 2000, auto_iter = FALSE, verbose = TRUE) %>%
             divergence()
    set.seed(1234)
    slda2 <- textmodel_lda(dfmt, k = k, max_iter = 2000, auto_iter = TRUE, verbose = TRUE) %>%
             divergence()
    set.seed(1234)
    slda3 <- textmodel_lda(dfmt, k = k, max_iter = 2000, auto_iter = FALSE, verbose = TRUE,
                           batch_size = 0.1) %>%
             divergence()
    set.seed(1234)
    slda4 <- textmodel_lda(dfmt, k = k, max_iter = 2000, auto_iter = TRUE, verbose = TRUE,
                           batch_size = 0.1) %>%
             divergence()
    tmp <- cbind(k, slda1, slda2, slda3, slda4)
    print(tmp)
    dat <- rbind(dat, tmp)
}

matplot(dat[1], dat[-1], type = "b")

