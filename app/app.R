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
      plotOutput("plot"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output, session) {
  fitted <- reactive({
    set.seed(42)
    n <- input$n
    n1 <- n %/% 2
    n2 <- n - n1
    # Two moderately separated Gaussian blobs
    x <- rbind(
      cbind(rnorm(n1, 0.30, 0.10), rnorm(n1, 0.30, 0.10)),
      cbind(rnorm(n2, 0.70, 0.10), rnorm(n2, 0.70, 0.10))
    )
    y <- factor(c(rep("A", n1), rep("B", n2)))
    list(
      model = svm(
        x, y,
        kernel = input$kernel,
        cost = 2^input$cost,
        type = "C-classification"
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
    pad <- 0.05
    xr <- range(fit$x[, 1]) + c(-pad, pad)
    yr <- range(fit$x[, 2]) + c(-pad, pad)
    xs <- seq(xr[1], xr[2], length.out = 80)
    ys <- seq(yr[1], yr[2], length.out = 80)
    g <- as.matrix(expand.grid(V1 = xs, V2 = ys))
    pred <- predict(m, g, decision.values = TRUE)
    z <- matrix(attr(pred, "decision.values"), length(xs), length(ys))

    pal <- hcl.colors(64, input$palette)
    class_cols <- pal[c(12, 52)]

    par(mar = c(4, 4, 2, 1))
    image(
      x = xs, y = ys, z = z,
      col = pal,
      xlab = expression(x[1]), ylab = expression(x[2]),
      main = paste("SVM decision –", m$kernel)
    )
    contour(xs, ys, z, levels = 0, add = TRUE, drawlabels = TRUE, col = "white", lwd = 2)
    contour(xs, ys, z, levels = c(-1, 1), add = TRUE, drawlabels = TRUE, col = "white", lwd = 1.5, lty = "44")

    pts <- fit$x
    sv <- m$index
    nonsv <- setdiff(seq_len(nrow(pts)), sv)
    cols <- class_cols[as.integer(fit$y)]

    if (length(nonsv)) {
      points(pts[nonsv, , drop = FALSE], pch = 21, cex = 1.0, bg = cols[nonsv], col = cols[nonsv], lwd = 1)
    }
    points(pts[sv, , drop = FALSE], pch = 21, cex = 1.4, bg = cols[sv], col = "white", lwd = 2)
  })
}

shinyApp(ui, server)
