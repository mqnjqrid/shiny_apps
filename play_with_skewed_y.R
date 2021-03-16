#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


ui <- fluidPage(
  
  # Application title
  titlePanel("Transform skewed data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("ydata",
                  label = "choose y data to transform:",
                  choices = list("y1",
                                 "y2",
                                 "y3",
                                 "y4",
                                 "y5",
                                 "y6"),
                  selected = "y1"),
      numericInput("alpha",
                   "alpha:",
                   #min = -10,
                   #max = 10,
                   value = 1),
      #animate = TRUE),
      numericInput("beta",
                   "beta:",
                   #min = -10,
                   #max = 10,
                   value = 0),
      #                  animate = TRUE),
      numericInput("pow",
                   "pow:",
                   #min = -10,
                   #max = 10,
                   value = 1),
      #animate = TRUE),
      selectInput("transform",
                  label = "choose the transformation for y*alpha + beta:",
                  choices = list("identity",
                                 "log",
                                 "power",
                                 "exp"
                  ),
                  selected = "identity")
    ),
    
    # Show a plot of the generated distribution
    mainPanel("original and transformed",
              fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph1"), plotOutput("plotgraph2"))
              )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # data
  x = rnorm(5000, 25, 10)
  epsilon = rnorm(5000, 0, 1)
  y <- abs((10 + 0.5*x + epsilon)/10)
  
  # generate bins based on input$bins from ui.R
  
  power2 <- reactive({input$pow
    powpow<- function(y){(y*input$alpha + input$beta)^input$pow}
    return(powpow)})
  
  yprime <- reactive({
    switch(input$ydata,
           "y1" = y,
           "y2" = y^2,
           "y3" = y^2 - 30,
           "y4" = log(y),
           "y5" = exp(y),
           "y6" = 1/y
           #"y7" = expit(3*y - 6.5)
           )
  })
  
  output$plotgraph1 = renderPlot({ y = yprime();hist(y)})
  output$plotgraph2 = renderPlot({
    y = yprime()
    power <- power2()
    
    if(input$transform == "power"){
      ytilde <- (y*input$alpha + input$beta)^input$pow
    }else{
      ytilde <- getFunction(input$transform)(y*input$alpha + input$beta)
    }
    
    hist(ytilde, breaks = 12)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
