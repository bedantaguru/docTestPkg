library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Hello Shiny!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot")

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({

    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")

  })

}

# Create Shiny app ----

#runApp(shinyApp(ui = ui, server = server), port = 6833L)

# nb : non blocking
# sa = shinyApp(ui = ui, server = server)
runApp_nb <- function(sa, pin_port = NULL){


  viewer <- getOption("viewer")
  if(!is.null(viewer)){

    if(!is.null(pin_port) && is.integer(pin_port)){
      port <- pin_port
    }else{
      port <- as.integer(sample(3000:49000, 1))
    }

    print(port)

    x <- callr::r_bg(function(x, y){shiny::runApp(x, port = y)}, args = list(sa , port))
    viewer(paste0("http://localhost:",port,"/"))
    return(invisible(x))
  }

  cat("\nThis is designed for RStudio\n")

  return(invisible(0))

}
runApp_nb(shinyApp(ui = ui, server = server))
#
# td <- tempdir()
# dir.create(td, recursive = T, showWarnings = F)
# saf <- tempfile("sa", tmpdir = td)
# saveRDS(shinyApp(ui = ui, server = server), saf)
#
x <- callr::r_bg(function(x){shiny::runApp(x, port = 6833L)},
                 args = list(shinyApp(ui = ui, server = server)))

rstudioapi::viewer("http://127.0.0.1:6833")

#
# x <- callr::r_bg(function(x,y){shiny::runApp(x, port = 6833L, launch.browser = y)}, args = list(shinyApp(ui = ui, server = server), getOption("viewer")))
#
#
