library(dplyr)
library(visNetwork)

library(scales)
dfm <- dfm(hashtags)
tdfm <- topfeatures(dfm,ntophashtag)
toptag <- names(topfeatures(dfm(hashtags),ntophashtag))
# creazione della matrice di cooccorrenza
tagfcm <- fcm(hashtags,tri=TRUE)

top_tagfcm <- fcm_select(tagfcm,pattern = toptag)
fcm_d <- convert(top_tagfcm,'data.frame')%>%pivot_longer(-document,names_to = "to",values_to = "width")
fcm_d <- fcm_d%>%
  mutate(from=document)%>%
  select(-document)%>%
  filter(width>0)


obj <- create_nodes_edges_htoh(hashtags,30)
grafo_htoh2(obj[1][[1]], obj[2][[1]])
nodes <- data.frame(id=names(tdfm)
                    ,size=rescale(tdfm,c(5,35))
                    ,title = paste0("<p>", names(tdfm)," - ",tdfm,"</p>")
                    ,stringsAsFactors = FALSE)


edges <- as.data.frame(fcm_d)%>%
  select(from,to,value=width)%>%
  filter(to!=from)%>% #Eliminiamo gli archi su stesso
  #filter(value>=20)%>% # Selezionaiamo solo rami con frequenze maggiori di 20
  mutate(title = paste0("<p>",from,"->",to," (",value,")","</p>")
  )



nodes <- data.frame(id=c(1,2,3,4,5,6,7),size=c(5,10,15,20,25,30,35))
edges <- data.frame(from=c(1,1,1,1,1,1),to=c(2,3,4,5,6,7),value=c(10,20,30,40,50,1000))
grafo_htoh2(nodes,edges)
