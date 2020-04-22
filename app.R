library(shiny)
library(scales)
library(knitr)
library(shinyWidgets)

server <- function(input, output, session){
    
mySample <- sample.int(n = nrow(diamonds),
                           size = 500, replace = FALSE)

sample_diamonds <- diamonds[mySample, ]
       
model_4 <- lm(log10(price) ~ log10(carat) + cut + clarity + color, 
              data = sample_diamonds)
    
prediction <- reactive({
    inputcarat <- input$carat
    inputcut <- input$cut
    inputclarity <- input$clarity
    inputcolor <- input$color
    
    MyNewDF <- data.frame(carat = inputcarat,
                        cut = inputcut,
                        clarity = inputclarity,
                        color = inputcolor)
    
    predict(model_4, newdata = MyNewDF,
            interval = "prediction", level = .95)
})

output$mycarat <- renderUI({
    sliderInput("carat", 
                "Select Carat", 
                min = 0, 
                max=10, 
                step=0.05, 
                value = 0.5)
})

output$mycut <- renderUI({
    selectInput("cut",
                "Select Cut",
                choices = c("Ideal","Premium","Very Good","Good","Fair"),
                selected = "Ideal")
})

output$mycolor <- renderUI({
    selectInput("color",
                "Select Color",
                choices = c("D", "E", "F", "G", "H", "I", "J"),
                selected = "D")
})

output$myclarity <- renderUI({
    selectInput("clarity",
                "Select Clarity",
                choices = c("IF","VVS1","VVS2","VS1","VS2","SI1", "SI2", "I1"),
                selected = "IF")
})

output$mypredict <- renderUI({
    textOutput("predict")
})

observe({
    output$predict <- renderText({
        paste("Estimated Price: ($)", round(10^(prediction())[1],2))
    })
})
}

rmdknit <- c("Final.rmd")
sapply(rmdknit, knit, quiet = T)

ui <- fluidPage(
    shinyUI(navbarPage("Diamonds",
                       theme = shinytheme("yeti"),
                       tabPanel("Report", includeMarkdown("Final.md")),
                       tabPanel("Price Estimation",
                                fluidPage(
                                    titlePanel("Estimated Diamond Price"),
                                    sidebarPanel(
                                        uiOutput("mycarat"),
                                        uiOutput("mycut"),
                                        uiOutput("mycolor"),
                                        uiOutput("myclarity"),
                                        uiOutput("myreset")),
                                    mainPanel(
                                        h2(strong(uiOutput("mypredict")))
                                        ))))))


shinyApp(ui, server)