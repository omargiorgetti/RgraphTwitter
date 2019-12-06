#####
# DASHBOARD SENTIMENT TWITTER
# Server
#####

#Using package shinydashboard
dashboardPage(
  dashboardHeader(title = "TwitterGraph",
                  dropdownMenuOutput("messageMenu")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Grafo", tabName = "grafo", icon = icon("project-diagram")),
      menuItem("Dati", tabName = "data", icon = icon("database")),
      menuItem("Help",icon=icon("info-circle"),href="https://github.com/omargiorgetti/RgraphTwitter")
    )
  ),
  dashboardBody(
    fluidRow(title="Rappresentazione tramite Grafo delle relazione tra hashtags in una collezione di Tweets sul cambiamento climatico"),
    fluidRow(
      tabItems(
        
        tabItem("grafo",
                visNetworkOutput("network")
        ),
        tabItem("data",
                fluidPage(
                  DT::dataTableOutput("table")
                )
        )
      )
    )
  )
)

