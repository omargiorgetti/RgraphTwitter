source("src/config.R")
source("src/global_graph.R")
source("src/global_preproc.R")

corpus <- create_corpus_csv("data/tweet201908.csv")
hashtags <-get_hashtags(corpus)
mentions <-get_mentions(corpus)

obj <- create_nodes_edges_htoh(hashtags,ntophashtag)
grafo_htoh1(obj[1][[1]],obj[2][[1]])


