library(yaml)
library(shiny)
#library(crosstalk)
library(bslib)
source('./R/get_dataset_loop.R')


charts <- list()

# if a tab name has been supplied use that alone 

#  create a list of all the bits of data we need: tabs and sections --
rules <- getRules()


state <- getState(rules, list())
for (t in lapply(rules$tabs, get_tab_data, state))  {
    for (u in t) {
        charts[[u[[1]]]] <- u[[2]]
    }
}


#  shiny modules to create cards displaying stats -------------------

uiLoop <- function(sector, tab_name) {
    ns <- NS(paste(tab_name, sector$name, sep='_'))
    tagList(
        accordion_panel( title=sector$title, plotlyOutput( ns( "chart" ))),
        accordion_panel( title='Description', sector$description )
    )
}

cardUi <- function(tab) {
  nav_panel(tab$name, tagList(lapply(tab$sections, uiLoop, tab$name)))
}

cardServer <- function(chartname) {
    moduleServer(
        chartname,
        function(input, output, session) {
            output$chart <-
                renderPlotly({
                    do.call(
                        charts[[chartname]]$ui_method,
                        list(charts[[chartname]]$data))
                })
        }
    )
}

# -----------------------------------------------------------------

ui <- page_fillable(
    do.call(navset_card_tab,
            lapply(rules$tabs, cardUi))
)

server <- function(input, output, session) {
    lapply(names(charts), cardServer)
}

shinyApp(ui, server)
