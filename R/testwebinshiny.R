

library(shiny)


ui <- fluidPage(titlePanel("Getting Iframe"),
                sidebarLayout(
                  sidebarPanel(
                    fluidRow(
                      column(6, textInput("st", label=h5("Search Me"))
                      ))),
                  mainPanel(fluidRow(
                    htmlOutput("frame")
                  )
                  )
                ))

server <- function(input, output) {
  observe({
    test <<- paste0("https://www.google.com/search?q=",input$st)
  })
  output$frame <- renderUI({
    input$st
    my_test <- tags$iframe(src=test, height=600, width=535)
    #print(my_test)
    my_test
  })
}

shinyApp(ui, server)
