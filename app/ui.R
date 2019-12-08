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
    fluidRow(
      tabItems(
        tabItem("grafo",
            box(title="Graph rapresentation hashtags relationship in a climate change collection tweets "
                 ,status = "primary"
            ,visNetworkOutput("network")
            )
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

