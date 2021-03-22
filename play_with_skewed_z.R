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
                  label = "choose z data to transform:",
                  choices = list("z1",
                                 "z2",
                                 "z3",
                                 "z4",
                                 "z5",
                                 "z6"),
                  selected = "z1"),
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
  z <- abs((10 + 0.5*x + epsilon)/10)
  
  # generate bins based on input$bins from ui.R
  
  power2 <- reactive({input$pow
    powpow<- function(z){(z*input$alpha + input$beta)^input$pow}
    return(powpow)})
  
  zprime <- reactive({
    switch(input$ydata,
           "z1" = exp(z) - 10,
           "z2" = exp(z),
           "z3" = z^2,
           "z4" = z^2 - 30,
           "z5" = log(z),
           "z6" = 1/z
           #"y7" = expit(3*y - 6.5)
           )
  })
  
  output$plotgraph1 = renderPlot({ z = zprime();hist(z)})
  output$plotgraph2 = renderPlot({
    require(gtools)
    z = zprime()
    power <- power2()
    
    if(input$transform == "power"){
      ztilde <- (z*input$alpha + input$beta)^input$pow
    }else{
      ztilde <- getFunction(input$transform)(z*input$alpha + input$beta)
    }
    
    hist(ztilde, breaks = 12)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
