# dato un lene
library(visNetwork)
library(tidyr)
create_nodes_edges_htoh <- function(hashtags,ntophashtag){
  # estrazione dei ntophasht hashtag priù frequenti creando la dfm 
  toptag <- names(topfeatures(dfm(hashtags),ntophashtag))
  # creazione della matrice di cooccorrenza
  tagfcm <- fcm(hashtags,tri=TRUE)
  # riduzione della matrice di cooccorrenza ai soli top hashtags 
  top_tagfcm <<- fcm_select(tagfcm,pattern = toptag)
  ssname<-top_tagfcm@Dimnames$features
  fcm_d <<- convert(top_tagfcm,'data.frame')%>%pivot_longer(-document,names_to = "to",values_to = "width")
  fcm_d <- fcm_d%>%
    mutate(from=document)%>%
    select(-document)%>%
    filter(width>0)

  nodes <- data.frame(id=ssname
                      ,title = paste0("<p>", ssname,"</p>")
                      ,stringsAsFactors = FALSE)
  nodes <- nodes%>%arrange(id)
  edges <- as.data.frame(fcm_d)%>%
    select(from,to,value=width)%>%
    filter(to!=from)%>% #Eliminiamo gli archi su stesso
    #filter(value>=20)%>% # Selezionaiamo solo rami con frequenze maggiori di 20
    mutate(title = paste0("<p>", value,"</p>")
    )
  ris <- list(nodes,edges)
  names(ris) <- c('nodes','edges')
  return(ris)
  
}

grafo_htoh1 <- function(nodes,edges){
  visNetwork(nodes, edges)%>%
    visPhysics(solver="barnesHut",stabilization = TRUE, barnesHut = list(gravitationalConstant=-50000))%>%
    visIgraphLayout(layout = "layout_nicely",randomSeed = 12) %>%
    # visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T)
    #,selectedBy = "group" )%>% 
    visEdges(smooth = FALSE,color=list(background='#848484',highlight='red'))%>%
    visNodes(label=NULL)%>%
    visOptions(nodesIdSelection = T,highlightNearest = list(enabled = T, degree = 1, hover = T))
}


grafo_htoh2 <- function(nodes,edges){
visNetwork(nodes, edges)%>%
  visPhysics(solver="forceAtlas2Based",stabilization = TRUE)%>%
  visIgraphLayout(layout = "layout_nicely",randomSeed = 1234) %>%
  # visIgraphLayout(physics=TRUE)%>%
  # visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T)
  #,selectedBy = "group" )%>% 
  visEdges(smooth = FALSE,color=list(background='#848484',highlight='red'))%>%
  visNodes(label=NULL)%>%
  visOptions(nodesIdSelection = T)
}
grafo_htoh3 <- function(nodes,edges){
visNetwork(nodes, edges)%>%
  visPhysics(solver="forceAtlas2Based")%>%
  visIgraphLayout()%>%
  visNodes(
    shape = "dot",
    color = list(
      background = "#0085AF",
      border = "#013848",
      highlight = "#FF8000"
    ),
    shadow = list(enabled = TRUE, size = 10)
  ) %>%
  visEdges(
    shadow = FALSE,
    color = list(color = "#0085AF", highlight = "#C62F4B")
  ) %>%
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T)
             #,selectedBy = "group" 
  )%>% 
  visLayout(randomSeed = 11)
}