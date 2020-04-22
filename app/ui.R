dashboardPage(
# Header
dashboardHeader(
  title = "TwitterGraph",
  dropdownMenuOutput("messageMenu")
),
# Sidebar
dashboardSidebar(
  #menu
  sidebarMenu(
    # creazion di un div con testo "Grafo" e id "grafo" e icona
    menuItem("Grafo"
             , tabName = "grafo"
             , icon = icon("project-diagram")
    ),
    # creazion di un div con testo "Dati" e id "data" e icona
    menuItem(
      "Dati"
      , tabName = "data"
      , icon = icon("database")
    ),
    # creazion di un href tag con testo "Help" e id "grafo" e icona
    menuItem(
      "Help"
      ,icon=icon("info-circle")
      ,href="https://github.com/omargiorgetti/RgraphTwitter"
    )
  )
),
# Body
dashboardBody(
  fluidRow(
    # Item che si attiva al click nella colonna Sidebar
    tabItems(
      # Al click sull'id "grafo" visualizza la rete
      # generata con il pacchetto visNetwork lato server
      # salvata nell'oggetto "network"
      tabItem("grafo",
              visNetworkOutput("network")
      ),
      # Al click sull'id "data" visualizza tramite la funzione
      # dataTableOutput del pacchetto DT, il data.frame
      # passato dal server nell'oggetto "table"
      tabItem("data",
              fluidPage(
                DT::dataTableOutput("table")
              )
      )
    )
  )
)
)

