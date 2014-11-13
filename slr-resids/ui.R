library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Residual Plots"),
  
  sidebarPanel(    
    
    sliderInput("n", label = h4("No. Observations"),
                min = 5, max = 250, value = 30),
    
    
    radioButtons("x", label = h4("Explanatory variable"),
                 choices = list("Discrete" = 1, "Continuous" = 2),
                 selected = 1),
    
    checkboxGroupInput("violations", label = h4("Violations"),
                       c("Nonlinearity" = "nonlinear",
                         "Non-constant variance" = "nonconstant")),
    
    
    actionButton("goButton", "Again")  
  ),
  
  mainPanel(
    
    fluidRow(
      column(8, 
             h4("The fitted model"),
             plotOutput("data"))
    ),
    fluidRow(
      column(8,
             h4("Residuals vs. fitted values"),
             plotOutput("fitted"))
    ),
    fluidRow(
      column(8,
             h4("Residuals vs. x"),
             plotOutput("x"))
    )
  )
))