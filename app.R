library(shiny)
library(data.table)
library(e1071)

msrm <- read.csv(file = "data/mushroom_factored.csv", stringsAsFactors = TRUE)

ui <- fluidPage(
  titlePanel("Mushroom Edibility Identification"),
  p("Is this mushroom safe to eat or a deadly poison?"),
  hr(),
  fluidRow(
    sidebarPanel(
      width = 2,
      selectInput(inputId = "odor", label = "odor", choices = levels(msrm$odor)),
      selectInput(inputId = "habitat", label = "habitat", choices = levels(msrm$habitat)),
      selectInput(inputId = "population", label = "population", choices = levels(msrm$population)),
      selectInput(inputId = "bruises", label = "bruises", choices = levels(msrm$bruises)),
      selectInput(inputId = "sporePrintColor", label = "sporePrintColor", choices = levels(msrm$spore.print.color)),
    ),
    sidebarPanel(
      width = 2,
      selectInput(inputId = "capShape", label = "capShape", choices = levels(msrm$cap.shape)), 
      selectInput(inputId = "capSurface", label = "capSurface", choices = levels(msrm$cap.surface)), 
      selectInput(inputId = "capColor", label = "capColor", choices = levels(msrm$cap.color)), 
      
      selectInput(inputId = "ringNumber", label = "ringNumber", choices = levels(msrm$ring.number)),
      selectInput(inputId = "ringType", label = "ringType", choices = levels(msrm$ring.type)), 
    ),
    
    sidebarPanel(
      width = 2,
      selectInput(inputId = "gillAttachment", label = "gillAttachment", choices = levels(msrm$gill.attachment)), 
      selectInput(inputId = "gillSpacing", label = "gillSpacing", choices = levels(msrm$gill.spacing)), 
      selectInput(inputId = "gillSize", label = "gillSize", choices = levels(msrm$gill.size)), 
      selectInput(inputId = "gillColor", label = "gillColor", choices = levels(msrm$gill.color)),
      
      selectInput(inputId = "veilColor", label = "veilColor", choices = levels(msrm$veil.color)),
    ),
    
    sidebarPanel(
      width = 2,
      selectInput(inputId = "stalkShape", label = "stalkShape", choices = levels(msrm$stalk.shape)), 
      selectInput(inputId = "stalkRoot", label = "stalkRoot", choices = levels(msrm$stalk.root)), 
      selectInput(inputId = "stalkSurfaceAboveRing", label = "stalkSurfaceAboveRing", choices = levels(msrm$stalk.surface.above.ring)), 
      selectInput(inputId = "stalkSurfaceBelowRing", label = "stalkSurfaceBelowRing", choices = levels(msrm$stalk.surface.below.ring)), 
      selectInput(inputId = "stalkColorAboveRing", label = "stalkColorAboveRing", choices = levels(msrm$stalk.color.above.ring)), 
      selectInput(inputId = "stalkColorBelowRing", label = "stalkColorBelowRing", choices = levels(msrm$stalk.color.below.ring)),
    ),
    
    column(
      width = 4,
      tags$label(h4("Output")),
      verbatimTextOutput("txtResult"),
      hr(),
      HTML("<p>Developed by Deddy Romnan Rumapea (2021).</p>
           <a href='#'>Read documentation.</a>"),
    )
  ),
)

server <- function(input, output, session) {
  model.svm <- readRDS("model/svm.RDS")
  
  output$txtResult <- renderText({
    start <- Sys.time()
    prediction <- predict(model.svm, input.df(input))
    executionTime <- Sys.time() - start
    
    paste("Model\t: Support Vector Machine (SVM)",
          "\nTime\t:", executionTime , "seconds",
          "\nResult\t:", prediction)
  })
}

input.df <- function(input){
  df <- data.frame(
    odor = factor(x = input$odor, levels = levels(msrm$odor)),
    habitat = factor(x = input$habitat, levels = levels(msrm$habitat)),
    population = factor(x = input$population, levels = levels(msrm$population)),
    bruises = factor(x = input$bruises, levels = levels(msrm$bruises)),
    veil.color = factor(x = input$veilColor, levels = levels(msrm$veil.color)),
    spore.print.color = factor(x = input$sporePrintColor, levels = levels(msrm$spore.print.color)),
    
    ring.number = factor(x = input$ringNumber, levels = levels(msrm$ring.number)),
    ring.type = factor(x = input$ringType, levels = levels(msrm$ring.type)),
    
    cap.shape = factor(x = input$capShape, levels = levels(msrm$cap.shape)),
    cap.surface = factor(x = input$capSurface, levels = levels(msrm$cap.surface)),
    cap.color = factor(x = input$capColor, levels = levels(msrm$cap.color)),
    
    gill.attachment = factor(x = input$gillAttachment, levels = levels(msrm$gill.attachment)),
    gill.spacing = factor(x = input$gillSpacing, levels = levels(msrm$gill.spacing)),
    gill.size = factor(x = input$gillSize, levels = levels(msrm$gill.size)),
    gill.color = factor(x = input$gillColor, levels = levels(msrm$gill.color)),
    stalk.shape = factor(x = input$stalkShape, levels = levels(msrm$stalk.shape)),
    
    stalk.root = factor(x = input$stalkRoot, levels = levels(msrm$stalk.root)),
    stalk.surface.above.ring = factor(x = input$stalkSurfaceAboveRing, levels = levels(msrm$stalk.surface.above.ring)),
    stalk.surface.below.ring = factor(x = input$stalkSurfaceBelowRing, levels = levels(msrm$stalk.surface.below.ring)),
    stalk.color.above.ring = factor(x = input$stalkColorAboveRing, levels = levels(msrm$stalk.color.above.ring)),
    stalk.color.below.ring = factor(x = input$stalkColorBelowRing, levels = levels(msrm$stalk.color.below.ring)),
    stringsAsFactors = FALSE
  )
  return(df)
}

shinyApp(ui = ui, server = server)