# dato un lene
create_nodes_edges_htoh <- function(hashtags,ntophashtag){
  # estrazione dei ntophasht hashtag priù frequenti creando la dfm 
  toptag_d <- topfeatures(dfm(hashtags),ntophashtag)
  # creazione della matrice di cooccorrenza 
  tagfcm <- fcm(hashtags,tri=TRUE)
  # riduzione della matrice di cooccorrenza ai soli top hashtags 
  hashtag_sel <- names(toptag_d)
  top_tagfcm <- fcm_select(tagfcm,pattern = hashtag_sel)
  
  fcm_d <- convert(top_tagfcm,'data.frame')%>%pivot_longer(-document,names_to = "to",values_to = "width")
  fcm_d <- fcm_d%>%
    mutate(from=document)%>%
    select(-document)%>%
    filter(width>0)
  
  nodes <- data.frame(id=hashtag_sel
                      ,size=rescale(toptag_d,c(5,35))
                      ,title = paste0("<p>", hashtag_sel," - ",toptag_d,"</p>")
                      ,label=NA
                      ,stringsAsFactors = FALSE)
  nodes <- nodes%>%arrange(id)
  edges <- as.data.frame(fcm_d)%>%
    select(from,to,value=width)%>%
    filter(to!=from)%>% #Eliminiamo gli archi su stesso
    #filter(value>=20)%>% # Selezionaiamo solo rami con frequenze maggiori di 20
    mutate(title = paste0("<p>",from,"->",to," (",value,")","</p>")
    )
  ris <- list(nodes,edges)
  names(ris) <- c('nodes','edges')
  return(ris)
  
}


create_nodes_edges_ttot <- function(){
  tt <- to_token(corpus08)
  
  hashtags <- attr(tokens_select(tt,"#*"),'types')
  dfmk <- dfm_keep(dfm(tt),hashtags)
  
  
  df_d <- convert(dfmk,to="data.frame")
  
  #ttot_graph_be <- function()
  nodes <- data.frame(id=dfmk@Dimnames$docs,label=dfmk@Dimnames$docs)#,stringsAsFactors = FALSE)
  
  colnames(df_d) <- c('doc',hashtags)
  df_ini <-gather(df_d,hashtags,key="hashtags",value = "pres")%>%filter(pres==1)
  
  df <- df_ini
  edges <- data.frame()
  #ciclo sui nodi che rappresentano i tweets
  for (node in unlist(nodes)){
    print(node)
    #ciclo sugli hashtag 
    for (h in hashtags){
      print(h)
      # troviamo per un dato tweet (nodo) quali altri tweet, contengono quel hashtag (h)
      # verifica che siano presenti i nodi con l'hashtag
      if (df%>%filter(hashtags==h & doc==node)%>%count()!=0){
        print('comb presente')
        # per quell'hashtag dobbiamo selezionare solo i nodi diversi da quello che stiamo lavorando
        # in questo modo evitiamo rami con se stesso, 
        # dalla tabella di riferimento per quell'hashtag escludiamo il nodo dato
        dd <- df%>%filter(hashtags==h & doc!=node)
        #verifichiamo che siano presenti nodi di destinazione
        if (dd%>%count()!=0){
          dd <- dd%>%select(to=doc)
          # creiamo un dataframe con i nodi rimanenti che sarrano la destinazione (to) e il nodo dato
          # che sarà la partenza (from). Inseriamo questi rami nella tabella
          edges<-rbind(edges,data.frame(from=node,to=dd,width=1))
          # eliminiamo la combinazione nodo hashtag, per evitare rami di ritorno 
          df <- df%>%filter(hashtags!=h | doc!=node)
          print(dd)
          print('df----------') 
          print(df)
          print('edge--------')
          print(edges)
        }
      }
      print('---------end---------------')  
    }
  }
  edges <- edges%>%group_by(from,to)%>%summarize(width=sum(width))
  
  
  
}

  
grafo_htoh <- function(nodes,edges){
  visNetwork(nodes, edges
            ,height = "800px",width = "100%"
    )%>%
    #visPhysics(solver="forceAtlas2Based",stabilization = TRUE)%>%
    visIgraphLayout(layout = "layout_with_fr",randomSeed = 1234) %>%
    # visIgraphLayout(physics=TRUE)%>%
    # visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T)
    #,selectedBy = "group" )%>% 
    visEdges(color=list(background='#848484',highlight='red',hover='green'))%>%
    visOptions(nodesIdSelection = 
                 list(
                   enabled=T
                   ,main="Scegliere l'hashtag"
                 )
               ,highlightNearest = list(
                 enabled = T
                 , degree = 1
                 , hover = T
               )
    )
}

grafo_htoh1 <- function(nodes,edges){
  visNetwork(nodes, edges)%>%
    visPhysics(solver="barnesHut",stabilization = TRUE, barnesHut = list(gravitationalConstant=-50000))%>%
    visIgraphLayout(layout = "layout_nicely",randomSeed = 12) %>%
    # visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T)
    #,selectedBy = "group" )%>% 
    visEdges(smooth = FALSE,color=list(background='#848484',highlight='red'))%>%
    visNodes(label=NULL,size=freq)%>%
    visOptions(nodesIdSelection = list(
        enabled=T
        ,main="Selezionare l'hashtag"
      )
      ,highlightNearest = list(
        enabled = T
        , degree = 1
        , hover = T
      )
    )
}



grafo_htoh3 <- function(nodes,edges){
visNetwork(nodes, edges,height = "100%",width = "100%")%>%
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
  visOptions(nodesIdSelection = list(enabled=T
                                     ,main="Scegliere l'hashtag"
                                     )
             ,highlightNearest = list(
               enabled = T
               , degree = 1
               , hover = T)
             #,selectedBy = "group" 
  )%>% 
  visLayout(randomSeed = 11)
}