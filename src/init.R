source("app/config.R")
source("app/conn.R")
source("app/global_graph.R")
source("app/global_preproc.R")
con <- connect("locale")

t <- select_tweet(con,drif,1)
t_e <- t%>%filter(topic=='energie_rinnovabili')
t_e%>%write_delim("../data/tweet201909.csv", delim=';')
tweets <- read_delim("data/tweet201909.csv", delim=';')
data <- tweets%>%filter(tweetdate==as.POSIXct('2019-09-27'))
corpus <- create_corpus(data)
hashtags <-get_hashtags(corpus)
#mentions <-get_mentions(corpus)

obj <- create_nodes_edges_htoh(hashtags,ntophashtag)



nodes <- obj$nodes
edges <- obj$edges
save(obj,data,hashtags,corpus,file="app/data.Rdata")

# nuemro token
length(hashtags)
dfm <- dfm(hashtags)
d <- dim(dfm)
sum(ntoken(dfm))
sum(ntype(dfm))
ww <- cbind(ntoken(dfm),ntype(dfm))
f <- fcm(dfm)
ftri <- fcm(dfm,tri=TRUE)
dim(f)
dim(ftri)

str(ww)

tokens(hashtags[497])
types(hashtags[497])


tokens(hashtags[500])
types(hashtags[500])

dfm@docvars%>%filter(docname_ %in% c('text500','text497'))%>%select(id)

dd1 <- tweets%>%filter(id=='1177580779477643264')%>%select(text)
dd2 <- tweets%>%filter(id=='1177523425436241920')%>%select(text)


ff <- fcm(dfm)
ff1 <- fcm(hashtags)

attributes(ntype(dfm))
aa <- summary(dfm)
#distinct type
length(dfm@Dimnames$features)
d[2]

typeof(toptag_d)
attributes(toptag_d)
