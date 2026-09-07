library(shiny)
library(bslib)
library(e1071)

ui <- fluidPage(
  theme = bs_theme(version = 3, bootswatch = "darkly"),
  titlePanel("SVM Surrogate Model"),
  sidebarLayout(
    sidebarPanel(
      selectInput("kernel", "Kernel", c("linear", "polynomial", "radial", "sigmoid")),
      sliderInput("cost", "Cost (log2)", min = 0, max = 4, value = 2, step = 1),
      sliderInput("n", "Training samples", min = 20, max = 200, value = 50, step = 10),
      actionButton("fit", "Fit SVM")
    ),
    mainPanel(
      verbatimTextOutput("summary"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  model <- reactiveVal(NULL)

  observeEvent(input$fit, {
    set.seed(42)
    n <- input$n
    x <- matrix(runif(n * 2), ncol = 2)
    y <- sin(x[, 1] * 2 * pi) + 0.3 * rnorm(n)

    m <- svm(x, y,
             kernel = input$kernel,
             cost = 2^input$cost,
             type = "eps-regression")
    model(m)
  })

  output$summary <- renderPrint({
    req(model())
    summary(model())
  })

  output$plot <- renderPlot({
    req(model())
    m <- model()
    # Prediction grid
    g <- expand.grid(
      V1 = seq(0, 1, length.out = 50),
      V2 = seq(0, 1, length.out = 50)
    )
    g$pred <- predict(m, as.matrix(g[, 1:2]))

    # Heatmap of predictions
    par(mar = c(4, 4, 2, 1))
    image(
      x = seq(0, 1, length.out = 50),
      y = seq(0, 1, length.out = 50),
      z = matrix(g$pred, 50, 50),
      col = hcl.colors(64, "YlGnBu"),
      xlab = "X1", ylab = "X2",
      main = paste("SVM predictions –", m$kernel)
    )
  })
}

shinyApp(ui, server)
