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
                                 "y6",
                                 "y7"),
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
                   "pow: (works only for power transformation)",
                   #min = -10,
                   #max = 10,
                   value = 1),
      #animate = TRUE),
      selectInput("transform",
                  label = "choose the transformation for y*alpha + beta:",
                  choices = list("identity",
                                 "log",
                                 "power",
                                 "exp",
                                 "logit"
                  ),
                  selected = "identity")
    ),
    
    # Show a plot of the generated distribution
    mainPanel("Scatter Plots",
              fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph1"), plotOutput("plotgraph2"))
              )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # data
  x = runif(500, 0, 50)
  epsilon = rnorm(500, 0, 1)
  y <- abs((10 + 0.5*x + epsilon)/10)
    
  # generate bins based on input$bins from ui.R
  
  #logit <- function(x){ if(x>0 & x<1) return(log(x/(1-x))) else return(x)}
  expit <- function(x){ return(exp(x)/(1+exp(x)))}
  
  power2 <- reactive({input$pow
    powpow<- function(y){(y*input$alpha + input$beta)^input$pow}
    return(powpow)})
  
  yprime <- reactive({
    switch(input$ydata,
           "y1" = exp(y)-10,
           "y2" = exp(y),
           "y3" = y^2,
           "y4" = y^2 - 30,
           "y5" = log(y),
           "y6" = 1/y,
           "y7" = expit(3*y - 6.5))
  })
  
  output$plotgraph1 = renderPlot({ y = yprime();plot(x, y, pch = 19, main = "Original")})
  output$plotgraph2 = renderPlot({
    library(gtools)
    y = yprime()
    power <- power2()
    
    if(input$transform == "power"){
      ytilde <- (y*input$alpha + input$beta)^input$pow
    }else{
      ytilde <- getFunction(input$transform)(y*input$alpha + input$beta)
    }
    
    plot(x, ytilde, pch = 19, main = "Transformed")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
