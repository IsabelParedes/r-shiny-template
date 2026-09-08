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
      selectInput("kernel", "Kernel", c("linear", "polynomial", "radial", "sigmoid")),
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
    x <- matrix(runif(n * 2), ncol = 2)
    y <- sin(x[, 1] * 2 * pi) + 0.3 * rnorm(n)
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
    xs <- seq(0, 1, length.out = 50)
    ys <- seq(0, 1, length.out = 50)
    g <- expand.grid(V1 = xs, V2 = ys)
    z <- matrix(predict(m, as.matrix(g)), 50, 50)

    par(mar = c(4, 4, 2, 1))
    image(
      x = xs, y = ys, z = z,
      col = hcl.colors(64, input$palette),
      xlab = "X1", ylab = "X2",
      main = paste("SVM predictions –", m$kernel)
    )
    contour(xs, ys, z, add = TRUE, drawlabels = TRUE, col = "white", lwd = 1.5)

    pts <- fit$x
    sv <- m$index
    yr <- range(fit$y, na.rm = TRUE)
    yi <- if (diff(yr) < 1e-9) {
      rep(50L, length(fit$y))
    } else {
      pmax(1L, pmin(100L, as.integer(round(1 + 99 * (fit$y - yr[1]) / diff(yr)))))
    }
    cols <- hcl.colors(100, input$palette)[yi]
    nonsv <- setdiff(seq_len(nrow(pts)), sv)

    if (length(nonsv)) {
      points(pts[nonsv, , drop = FALSE], pch = 16, cex = 0.9, col = cols[nonsv])
    }
    points(pts[sv, , drop = FALSE], pch = 21, cex = 1.3, bg = cols[sv], col = "white", lwd = 1.5)
  })
}

shinyApp(ui, server)
