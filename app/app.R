library(shiny)
library(bslib)
library(e1071)
library(thematic)

thematic_shiny(font = "auto")

ui <- page_fluid(
  theme = bs_theme(version = 5),
  input_dark_mode(mode = "light"),
  titlePanel("SVM Demo"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "kernel", "Kernel",
        c("linear", "polynomial", "radial", "sigmoid"),
        selected = "radial"
      ),
      sliderInput("cost", "Cost (log2)", min = 0, max = 4, value = 2, step = 1),
      sliderInput("n", "Training samples", min = 20, max = 200, value = 50, step = 10),
      selectInput(
        "palette",
        "Color palette",
        c("YlGnBu", "Viridis", "Plasma", "Inferno", "Blues", "RdYlBu", "Spectral"),
        selected = "Plasma"
      )
    ),
    mainPanel(
      verbatimTextOutput("summary"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  fitted <- reactive({
    set.seed(42)
    n <- input$n
    # Even coverage of the unit square + smooth 2D target (low noise)
    x <- matrix(runif(n * 2), ncol = 2)
    y <- exp(-((x[, 1] - 0.5)^2 + (x[, 2] - 0.5)^2) / (2 * 0.15^2)) +
      0.03 * rnorm(n)
    list(
      model = svm(
        x, y,
        kernel = input$kernel,
        cost = 2^input$cost,
        type = "eps-regression"
      ),
      x = x,
      y = y
    )
  })

  output$summary <- renderPrint({
    summary(fitted()$model)
  })

  output$plot <- renderPlot({
    fit <- fitted()
    m <- fit$model
    xs <- seq(0, 1, length.out = 80)
    ys <- seq(0, 1, length.out = 80)
    g <- expand.grid(V1 = xs, V2 = ys)
    z <- matrix(predict(m, as.matrix(g)), length(xs), length(ys))

    pal <- hcl.colors(64, input$palette)

    par(mar = c(4, 4, 2, 1))
    image(
      x = xs, y = ys, z = z,
      col = pal,
      xlab = "X1", ylab = "X2",
      main = paste("SVM predictions –", m$kernel)
    )
    contour(xs, ys, z, add = TRUE, drawlabels = TRUE, col = "white", lwd = 1.5)

    pts <- fit$x
    sv <- m$index
    nonsv <- setdiff(seq_len(nrow(pts)), sv)

    if (length(nonsv)) {
      points(pts[nonsv, , drop = FALSE], pch = 21, cex = 0.8, bg = "grey15", col = "grey60", lwd = 1.5)
    }
    points(pts[sv, , drop = FALSE], pch = 21, cex = 1.35, bg = "grey15", col = "white", lwd = 2)
  })
}

shinyApp(ui, server)
