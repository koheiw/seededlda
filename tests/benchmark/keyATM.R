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

colSums(dfm_lookup(dfmt, dict))

#data(keyATM_data_bills)
#bills_keywords <- keyATM_data_bills$keywords
#bills_dfm <- keyATM_data_bills$doc_dfm
dfmt <- dfmt[ntoken(dfmt) > 0,]
#keyATM_docs <- keyATM_read(bills_dfm)
#keyATM_docs <- keyATM_read(news_dfm)

wlda <- weightedLDA(docs = keyATM_read(dfmt), model = "base",
                    number_of_topics = 20)

slda <- textmodel_lda(dfmt, k = 20, max_iter = 1500, auto_iter = FALSE,
                      verbose = TRUE)
slda2 <- textmodel_lda(dfmt, k = 20, max_iter = 1500, auto_iter = TRUE,
                       verbose = TRUE)

as.matrix(top_words(wlda, 6))
terms(slda, 6)
terms(slda2, 6)


microbenchmark::microbenchmark(
    wlda = weightedLDA(docs = keyATM_read(dfmt), model = "base",
                        number_of_topics = 20),
    slda = textmodel_lda(dfmt, k = 20, max_iter = 1500, auto_iter = FALSE,
                          verbose = TRUE),
    times = 1
)

