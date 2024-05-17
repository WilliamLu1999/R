library(datasets)
library(shiny)
data("airquality")
ui <- fluidPage(
  titlePanel("Ozone Level"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId="bins",
                  label='Number of bins:',
                  min=1,
                  max=20,
                  value=30,
                  step=2)
    ),
    mainPanel(
      plotOutput(outputId = 'distPlot') # plot historgram
    )

  )
)

server <- function(input,output){
  output$distPlot <- renderPlot({
    x <- airquality$Ozone
    x <- na.omit(x) # omit missing values
    bins <- seq(min(x),max(x),length.out=input$bins+1)
    hist(x,breaks=bins,col='orange',border='black',xlab='ozone level',
         main='Histogram of Ozone')
  })
}
shinyApp(ui=ui,server=server)