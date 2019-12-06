#####
# DASHBOARD SENTIMENT TWITTER
# Server
#####
library(DT)

function(input, output, session) {
  output$messageMenu <- renderMenu({
    
    dropdownMenu(type = "notifications",
                 notificationItem(
                   text = "Author: Omar Giorgetti",
                   icon("user"),
                   status = "success"
                 ),
                 notificationItem(
                   text = "Source code",
                   icon("github"),
                   href="https://github.com/omargiorgetti/RgraphTwitter",
                   status = "danger"
                 ),
                 notificationItem(
                   text="Twitter",
                   icon("twitter"),
                   href="https://twitter.com/omargiorgetti",
                   status="info"
                 ),
                 notificationItem(
                   text="Linkedin",
                   icon("linkedin"),
                   href="https://www.linkedin.com/in/omargiorgetti/",
                   status="info"
                 )
                 
    )
  })
   # Show the raw data in the form of a data.table
  output$table <- DT::renderDataTable({
    table <- corpus$documents%>%
      mutate(link=str_c("<a target='_blank' class='fab fa-twitter' href='http://www.twitter.com/",user_screen_name,"/status/",id,"'></a>"),
             data=str_glue("{format(tweetdatetime,'%d/%b/%Y %H:%M:%S')}"))%>%
      select(link
             ,testo=texts
             ,profilo=user_screen_name
             ,data)%>%
      arrange(data)
    rownames(table) <- NULL
    table
  },escape=FALSE,rownames=FALSE
    )
  
  output$network <- renderVisNetwork({
    grafo_htoh3(obj[1][[1]],obj[2][[1]])
  })
}


