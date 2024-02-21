# Define server logic 
server <- function(input, output) {
  
  # create the plot
  output$plot <- renderPlot({
    plot_type <- switch(input$plot_type,
                        "Histogram" = create_histogram(input$x_axis),
                        "Boxplot" = create_boxplot(input$x_axis, input$y_axis),
                        "Scatterplot" = create_scatterplot(input$x_axis, input$y_axis),
                        "Map" = create_map(input$x_axis, input$y_axis))
    
    if (!is.null(plot_type)) {
      print(plot_type)
    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n", ann = FALSE)
      text(0.5, 0.5, "Invalid selection for plot type or variables.", cex = 1.2)
    }
  })
  
  # create histogram
  create_histogram <- function(x_var) {
    ggplot(public_data, aes_string(x = x_var)) + 
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") + 
      labs(x = x_var, y = "Frequency", title = "Histogram of Public Assistance Data") +
      theme_minimal() +
      theme(plot.title = element_text(color = "blue", size = 18, face = "bold"))
  }
  
  # create boxplot
  create_boxplot <- function(x_var, y_var) {
    ggplot(public_data, aes_string(x = x_var, y = y_var)) + 
      geom_boxplot(fill = "lightgreen", color = "black") + 
      labs(x = x_var, y = y_var, title = "Boxplot of Public Assistance Data") +
      theme_minimal() +
      theme(plot.title = element_text(color = "green", size = 18, face = "bold"))
  }
  
  # create scatterplot
  create_scatterplot <- function(x_var, y_var) {
    ggplot(public_data, aes_string(x = x_var, y = y_var)) + 
      geom_point(color = "red") + 
      labs(x = x_var, y = y_var, title = "Scatterplot of Public Assistance Data") +
      theme_minimal() +
      theme(plot.title = element_text(color = "red", size = 18, face = "bold"))
  }
  
  # create map
  create_map <- function(x_var, y_var) {
    map <- get_map(location = 'USA', zoom = 8)
    ggmap(map) +
      geom_point(data = public_data, aes_string(x = x_var, y = y_var), color = "red", alpha = 0.7) +
      labs(x = x_var, y = y_var, title = "Map of Public Assistance Data") +
      theme_minimal() +
      theme(plot.title = element_text(color = "red", size = 18, face = "bold"))
  }
}
