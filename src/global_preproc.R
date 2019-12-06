ctochar<-function(x){
  h <- str_replace(x,"[c]\\(",'')
  h <- str_replace_all(h,"\\'",'')
  h <- str_replace(h,"\\)",'')
  h <- str_trim(h)
return(h)
}
# restituisce un oggetto lubridate::interval: 
# - con num_months=1 il mese precendente la data a parametro (date='20/09/2019' risultato ('01/08/2019','31/08/2019')
# - con num_months=3 i tre mesi precedenti la data a parametero (date='20/09/2019' risultato ('01/06/2019','31/08/2019')
get_interval <- function(date,num_months){
  fd <- floor_date(date,unit="month") 
  di <- fd %m-% months(num_months)
  df <- fd %m-% days(1)
  return(interval(di,df))
}

# restituisce tutti i tweet della tabella 'tweets_dashboard' in formato dataframe con le seguenti variabili
# - id: id del tweet
# - text: testo de tweet
# - user_screen_name: profile che ha inviato il tweet
# - tweetdatetime: data e ora del tweet
# - tweetdate: data del tweet
# - tweettime: ora del tweet (hh:mm:ss)
# - tweethour: ora del tweet
# - tweetday: giorno del tweet
# - tweetmonth: mese del tweet
# - tweetyear: anno del tweet
trasform_tweet <- function(tweets){
  tweet_s <- tweets%>%select(id,text,user_screen_name,timestamp_ms)
  tweet_d <- tweet_s%>%collect() # carichiamo i dati dei tweet altrimenti non funziona la conversione a POSIX
  tweet_d <- tweet_d%>%
    mutate(tweetdatetime=  as.POSIXct(timestamp_ms,origin = '1970-01-01 00:00.00 UTC'))%>%
    mutate(
      tweetdate=as_date(tweetdatetime)
      ,tweettime=hour(tweetdatetime)
      ,tweettime=hour(tweetdatetime)
      ,day=day(tweetdatetime)
      ,month=month(tweetdatetime)
      ,year=year(tweetdatetime)
    )
  tweet_d <<- tweet_d%>%select(-timestamp_ms) #  eliminiamo timestamp_ms
}

# con: data source
select_tweet <- function(con){  
  tweets<-tbl(con,"tweets_dashboard")
  #sele dei metadati
  return(trasform_tweet(tweets))
}

# corpus su tutti i tweet della tabella 'tweets_dashboard'
create_corpus_db <- function(con){
  # selezione dei tweet da database e trasformazione 
  tweet <- select_tweet(con)
  corpus<- tweet%>%corpus()
  
  return(corpus)
}

# corpus di un dataframe
create_corpus <- function(dataframe){
  
    corpus<- dataframe%>%corpus()
  
  return(corpus)
}

# daterif: string data di riferimento
# creazione una lista con due elementi: 
# - il corpus con i tweet del mese presendente alla data di esecuzione
# - il corpus con i tweet dei tre mesi presendenti alla data di esecuzione
create_corpus_period <- function(daterif,con){
  # selezione dei tweet da database e trasformazione 
  corpus <- create_corpus(con)
  daterif <- as.POSIXct(daterif)
  corpus_month<- corpus%>%corpus_subset(tweetdate %within% get_interval(daterif,1))
  corpus_3months<-corpus%>%corpus_subset(tweetdate %within% get_interval(daterif,3))
  return(list(corpus_month,corpus_3months))
}

# creazione del copus partendo da un file csv
create_corpus_csv <- function(file){
  # selezione dei tweet da database e trasformazione 
  tweet <- read_delim(file, delim=';')  
  corpus<- tweet%>%corpus()
  return(corpus)
}

# funzione che restituisce un dataframe con le stopword nelle colonne
# - word
# - hpreword: stopword precedute da #
get_sw <- function(){
  #consideriamo le stopword di tidytext che comprendono quelle di quanteda e tm
  stopword_tidy <- 
    tidytext::get_stopwords(language='it',source="stopwords-iso")%>%
    mutate(hpreword=paste0('#',word))
  return(stopword_tidy)
}

# creaiamo il corpus inserendo alcune informazioni aggiuntive
## conversione del timestram per calcolare data, data e ora, e ora

# restituisce tutti i tokens ripuliti dato un oggetto corpus:
# - punteggiatura
# - sumboli
# - numeri
# - Url
# - stop word italiane
# - hastag che corrispondono a stopwords
get_tokens <- function(corpus_,ngrams_){
  sw <- get_sw()
  
  tokens<-tokens(corpus_,
                 remove_punct = TRUE,
                 remove_symbols = TRUE, 
                 remove_numbers=TRUE, 
                 remove_url = TRUE,  
                 ngrams=ngrams_)%>%
  # eliminazione dei carattri speciali QT e COMMENT
    tokens_remove("QT")%>%
    tokens_remove("COMMENT")%>%
  # riduzione a minuscolo 
              tokens_tolower()%>%
  # eliminazione le stopword italiane
      tokens_remove(sw$word)%>%
  # eliminazioni di hashtag che corrispondono a stopword, per esempio: #della
      tokens_remove(sw$hpreword)
    
  return(tokens)
}  

# restituisce gli hashtag dato un oggetto token
# token_select ha come input un oggetto token non può essere applicato
# direttamente all'oggetto corpus
get_hashtags_from_tokens<- function(tokens_){
  # individuiamo gli hashtag presenti nei tweet
  hashtag <- tokens_%>%tokens_tolower()%>%tokens_select("#*")
  # selezione degli hashtag
  return(hashtag)
}
# restituisce le mensioni dato un oggetto token
get_mentions_from_tokens <- function(tokens_){
  # individuiamo gli hashtag presenti nei tweet
  hashtag <- tokens_%>%tokens_select("@*")
  # selezione degli hashtag
  return(hashtag)
}

# restituisce features senza menzioni dato un oggetto token
get_tokens_without_mentions <- function(tokens_){
  # individuiamo gli hashtag presenti nei tweet
  hashtag <- tokens_%>%tokens_remove("@*")
  # selezione degli hashtag
  return(hashtag)
}

# restituisce features senza hashtag dato un oggetto token
get_tokens_without_hashtags <- function(tokens_){
  # individuiamo gli hashtag presenti nei tweet
  hashtag <- tokens_%>%tokens_remove("#*")
  # selezione degli hashtag
  return(hashtag)
}

# restituisce features senza menzioni e hashtag dato un oggetto token
get_tokens_without_mentions_hashtag <- function(tokens_){
  # individuiamo gli hashtag presenti nei tweet
  hashtag <- get_tokens_without_mentions(tokens_)%>%get_tokens_without_hashtags()
  # selezione degli hashtag
  return(hashtag)
}

# restituisce gli hashtags da un corpus
# tramite la funzione tokens() effettua la tokenizzazione.
# non viene chiamata la get_tokens() perchè è iniutile fare il prerpocessing
get_hashtags <- function(corpus_){
  get_hashtags_from_tokens(tokens(corpus_,ngrams=1))
}

# restituisce le menzioni da un corpus
# tramite la funzione tokens() effettua la tokenizzazione.
# non viene chiamata la get_tokens() perchè è iniutile fare il prerpocessing
get_mentions <- function(corpus_){
  get_mentions_from_tokens(tokens(corpus_,ngrams=1))
}

# restituisce gli hashtag utilizzati per estrarre i tweet
get_hashtags_rif <- function(con_){
  hashtag<-tbl(con,"topic_hashtag")
  h <- hashtag%>%collect()
  hashtag <-strsplit(as.character(ctochar(h$hashtags)),', ')[[1]]%>%str_replace_all('"','')
  return(hashtag)
}

# restituisce la colonna hashtag del db che contiene gli hastags registrati in fase di streaming
get_hashtags_from_db <- function(con_){
  tweets <- tbl(con,'tweet_dashboard')
  # selezione degli hashtag
  hashtag <- tweets%>%select(id,hashtag) %>%collect()
  # hashtag presenti tra quelli identificati del topic
  
  return(hashtag)
}


# Restitiuisce una lista di oggetti hashtags data una lista di corpus 
get_hashtags_m_corpus <- function(corpus_){
  val <- list()
  for (corpus in corpus_){
    tokens <- tokens(corpus,ngrams=1)
    val_el <- get_hashtags_from_tokens(tokens)
    val <- c(val,list(val_el))
  }
  return(val)
}
# Restitiuisce una lista di oggetti mentions data una lista di corpus 
get_mentions_m_corpus <- function(corpus_){
  val <- list()
  for (corpus in corpus_){
    tokens <- tokens(corpus,ngrams=1)
    val_el <- get_mentions_from_tokens(tokens)
    val <- c(val,list(val_el))
  }
  return(val)
}
