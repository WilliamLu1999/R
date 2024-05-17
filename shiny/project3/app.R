library(shiny)
library(caret)

# Assume 'tuned_model' is already trained and available in your environment
# If it's not in the current environment, you might need to load it from where it's saved:
# load("path/to/your/tuned_model.RData")

# Define UI
ui <- fluidPage(
  titlePanel("Iris Species Prediction with Tuned Random Forest"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("SepalLength", "Sepal Length", min = 4, max = 8, value = 5.5),
      sliderInput("SepalWidth", "Sepal Width", min = 2, max = 4.5, value = 3),
      sliderInput("PetalLength", "Petal Length", min = 1, max = 7, value = 4),
      sliderInput("PetalWidth", "Petal Width", min = 0.1, max = 2.5, value = 1.3),
      actionButton("predictButton", "Predict")
    ),
    mainPanel(
      HTML("<h3>Your Flower in mind is:</h3>"),
      textOutput("prediction")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$predictButton, {
    newdata <- data.frame(
      Sepal.Length = input$SepalLength,
      Sepal.Width = input$SepalWidth,
      Petal.Length = input$PetalLength,
      Petal.Width = input$PetalWidth
    )
    # Use the tuned model for prediction
    pred <- predict(tuned_model, newdata)
    output$prediction <- renderText({
      paste("Predicted species:", pred)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
