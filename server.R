library(plotly)
library(shiny)

server <- function(input, output) {
  # Create the Sierpinski plot
  output$sierpinski_plot <- renderPlotly({
    n_iterations <- input$iterations
    point_color <- input$point_color
    dot_size <- input$dot_size
    dot_transparency <- input$dot_transparency
    
    trace_point <- runif(3)
    corners <- data.frame(
      x = c(0, 1, 0.5, 0.5),
      y = c(0, 0, 1, 0.5),
      z = c(0, 0, 0, 1)
    )
    new_points <- data.frame(
      x = rep(0, n_iterations),
      y = rep(0, n_iterations),
      z = rep(0, n_iterations)
    )
    
    for (i in 1:n_iterations) {
      trace_point <- (corners[sample(4, 1), ] + trace_point) / 2
      new_points[i, ] <- trace_point
    }
    
    p <- plot_ly(
      data = new_points,
      x = ~x, y = ~y, z = ~z,
      type = "scatter3d",
      mode = "markers",
      marker = list(size = dot_size, color = point_color, opacity = dot_transparency)
    ) %>% 
      layout(scene = list(aspectmode = "cube"))
    
    p
  })
}
