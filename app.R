library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Statistical Distributions Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dist_type", "Select Distribution Type:",
                  choices = list(
                    "Discrete" = c("Binomial", "Poisson", "Geometric"),
                    "Continuous" = c("Normal", "Exponential", "Uniform")
                  ),
                  selected = "Binomial"),
      uiOutput("param_input")
    ),
    mainPanel(
      plotOutput("dist_plot"),
      verbatimTextOutput("dist_info")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive UI for parameters based on distribution type
  output$param_input <- renderUI({
    switch(input$dist_type,
           "Binomial" = list(
             numericInput("size", "Size (n):", value = 10, min = 1),
             numericInput("prob", "Probability (p):", value = 0.5, min = 0, max = 1)
           ),
           "Poisson" = list(
             numericInput("lambda", "Rate (lambda):", value = 3, min = 0)
           ),
           "Geometric" = list(
             numericInput("prob", "Probability (p):", value = 0.5, min = 0, max = 1)
           ),
           "Normal" = list(
             numericInput("mean", "Mean (mu):", value = 0),
             numericInput("sd", "Standard Deviation (sigma):", value = 1, min = 0)
           ),
           "Exponential" = list(
             numericInput("rate", "Rate (lambda):", value = 1, min = 0)
           ),
           "Uniform" = list(
             numericInput("min", "Minimum (a):", value = 0),
             numericInput("max", "Maximum (b):", value = 1)
           )
    )
  })
  
  # Plotting the distribution
  output$dist_plot <- renderPlot({
    dist_type <- input$dist_type
    x_vals <- seq(-10, 10, length.out = 1000)
    
    df <- switch(dist_type,
                 "Binomial" = data.frame(x = 0:input$size, y = dbinom(0:input$size, input$size, input$prob)),
                 "Poisson" = data.frame(x = 0:20, y = dpois(0:20, input$lambda)),
                 "Geometric" = data.frame(x = 0:20, y = dgeom(0:20, input$prob)),
                 "Normal" = data.frame(x = x_vals, y = dnorm(x_vals, input$mean, input$sd)),
                 "Exponential" = data.frame(x = x_vals[x_vals >= 0], y = dexp(x_vals[x_vals >= 0], input$rate)),
                 "Uniform" = data.frame(x = x_vals, y = dunif(x_vals, input$min, input$max))
    )
    
    ggplot(df, aes(x, y)) +
      geom_line() +
      labs(title = paste("Plot of", dist_type, "Distribution"),
           x = "X Values", y = "Density/Probability") +
      theme_minimal()
  })
  
  # Displaying distribution information
  output$dist_info <- renderPrint({
    switch(input$dist_type,
           "Binomial" = cat("PMF: dbinom(x, n, p)\nSupport: x = 0, 1, ..., n\nParameters: n (size), p (probability)\nMean: np\nVariance: np(1-p)\nMGF: (1 - p + p*e^t)^n\n"),
           "Poisson" = cat("PMF: dpois(x, lambda)\nSupport: x = 0, 1, 2, ...\nParameter: lambda (rate)\nMean: lambda\nVariance: lambda\nMGF: exp(lambda * (e^t - 1))\n"),
           "Geometric" = cat("PMF: dgeom(x, p)\nSupport: x = 0, 1, 2, ...\nParameter: p (probability)\nMean: 1/p\nVariance: (1-p)/p^2\nMGF: p*e^t / (1 - (1-p)*e^t) for t < -log(1-p)\n"),
           "Normal" = cat("PDF: dnorm(x, mu, sigma)\nSupport: x in (-∞, ∞)\nParameters: mu (mean), sigma (standard deviation)\nMean: mu\nVariance: sigma^2\nMGF: exp(mu*t + 0.5*sigma^2*t^2)\n"),
           "Exponential" = cat("PDF: dexp(x, lambda)\nSupport: x >= 0\nParameter: lambda (rate)\nMean: 1/lambda\nVariance: 1/lambda^2\nMGF: lambda / (lambda - t) for t < lambda\n"),
           "Uniform" = cat("PDF: dunif(x, a, b)\nSupport: x in [a, b]\nParameters: a (min), b (max)\nMean: (a + b)/2\nVariance: (b - a)^2 / 12\nMGF: (e^(bt) - e^(at)) / (t(b - a)) for t != 0\n")
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
