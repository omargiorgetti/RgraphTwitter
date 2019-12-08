source("../app/config.R")
source("../app/global_graph.R")
source("../app/global_preproc.R")

tweets <- read_delim("../data/tweet201908.csv", delim=';')
data <- trasform_tweet(tweets)
corpus <- create_corpus(data)
hashtags <-get_hashtags(corpus)
mentions <-get_mentions(corpus)

obj <- create_nodes_edges_htoh(hashtags,ntophashtag)

save(obj,data,hashtags,corpus,file="../app/data.Rdata")

