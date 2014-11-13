library(shiny)
library(ggplot2)

scatterplot <- function(x,y, title="", xlab="", ylab="") {
  d <- data.frame(x=x,y=y)
  p <- ggplot(d, aes(x=x,y=y)) + 
    geom_point() + 
    ggtitle(title) + 
    xlab(xlab) + 
    ylab(ylab)
  return(p)
}

simData <- function(n, x, violations) {
  linear <- !"nonlinear" %in% violations
  constant.var <- !"nonconstant" %in% violations
  
  intercept <- rnorm(1, sd = 2)
  slope     <- rnorm(1, sd = 2)
  slope2 <- rnorm(1)
  
  if(x == 1){
    x <- rep(0:5, length.out = n)
  } else{
    x <- runif(n, min = 0, max = 5)
  }
  
  if(linear == TRUE) {
    mu <- intercept + slope * x
  } else{
    mu <- intercept + slope * x + slope2 * x^2
  }
  
  if(constant.var == TRUE){
    e <- rnorm(n)
  } else{
    j <- sample(2, 1)
    if(j == 1){
      e <- rnorm(n) * sqrt(x + .1)
    } else{
      e <- rnorm(n) * sqrt(5.1 - x)
    }
  }
  
  y <- mu + e
  
  return(data.frame(x, y))
}


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
   data <- reactive({
     input$goButton
     simData(n = input$n, x = input$x, violations = input$violations)
   })
   
   mod <- reactive({
     lm(y~x, data = data())
   })
  
  output$data <- renderPlot({
    df <- data()
    qplot(x, y, data = df, geom = c("point", "smooth"), method = "lm", se = FALSE) 
  })
  
  
  output$fitted <- renderPlot({
    scatterplot(x = fitted(mod()), y = resid(mod()), xlab = "Fitted values", ylab = "Residuals") +
      geom_hline(yintercept=0)
  })
  
  output$x <- renderPlot({
    x <- model.frame(mod())$x
    r <- resid(mod())
    scatterplot(x = x, y = r, xlab = "x", ylab = "Residuals") +
      geom_hline(yintercept=0)
  })
})