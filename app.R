library(shiny)
library(data.table)
library(e1071)

msrm <- read.csv(file = "data/mushroom_factored.csv", stringsAsFactors = TRUE)

ui <- fluidPage(
  titlePanel("Mushroom Edibility Identification"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "odor", label = "Odor", choices = levels(msrm$odor)),
      selectInput(inputId = "habitat", label = "Habitat", choices = levels(msrm$habitat)),
      selectInput(inputId = "population", label = "Population", choices = levels(msrm$population)),
      selectInput(inputId = "bruises", label = "Bruises", choices = levels(msrm$bruises)),
      selectInput(inputId = "veilColor", label = "Veil color", choices = levels(msrm$veil.color)),
      selectInput(inputId = "sporePrintColor", label = "Spore print color", choices = levels(msrm$spore.print.color)),
      
      selectInput(inputId = "ringNumber", label = "Number of rings", choices = levels(msrm$ring.number)),
      selectInput(inputId = "ringType", label = "Ring type", choices = levels(msrm$ring.type)), 
      
      selectInput(inputId = "stalkShape", label = "stalkShape", choices = levels(msrm$stalk.shape)), 
      selectInput(inputId = "stalkRoot", label = "stalkRoot", choices = levels(msrm$stalk.root)), 
      selectInput(inputId = "stalkSurfaceAboveRing", label = "stalkSurfaceAboveRing", choices = levels(msrm$stalk.surface.above.ring)), 
      selectInput(inputId = "stalkSurfaceBelowRing", label = "stalkSurfaceBelowRing", choices = levels(msrm$stalk.surface.below.ring)), 
      selectInput(inputId = "stalkColorAboveRing", label = "stalkColorAboveRing", choices = levels(msrm$stalk.color.above.ring)), 
      selectInput(inputId = "stalkColorBelowRing", label = "stalkColorBelowRing", choices = levels(msrm$stalk.color.below.ring)), 
      
      selectInput(inputId = "capShape", label = "capShape", choices = levels(msrm$cap.shape)), 
      selectInput(inputId = "capSurface", label = "capSurface", choices = levels(msrm$cap.surface)), 
      selectInput(inputId = "capColor", label = "capColor", choices = levels(msrm$cap.color)), 
      
      selectInput(inputId = "gillAttachment", label = "gillAttachment", choices = levels(msrm$gill.attachment)), 
      selectInput(inputId = "gillSpacing", label = "gillSpacing", choices = levels(msrm$gill.spacing)), 
      selectInput(inputId = "gillSize", label = "gillSize", choices = levels(msrm$gill.size)), 
      selectInput(inputId = "gillColor", label = "gillColor", choices = levels(msrm$gill.color)),
      
      actionButton(inputId = "btnSubmit", label = "Submit", class = "btn btn-primary")
    ),
    
    mainPanel(
      tags$label(h3('Output')),
      verbatimTextOutput('contents'),
      verbatimTextOutput('textResult'),
      verbatimTextOutput("txtout")
    )
  )
)

server <- function(input, output, session) {
  datasetInput <- function(){
    df <- data.frame(
      Name = c("cap.shape", "cap.surface", "cap.color", "bruises", 
               "odor", "gill.attachment", "gill.spacing", "gill.size", 
               "gill.color", "stalk.shape", "stalk.root", 
               "stalk.surface.above.ring", "stalk.surface.below.ring", 
               "stalk.color.above.ring", "stalk.color.below.ring", "veil.color", 
               "ring.number", "ring.type", "spore.print.color", "population", 
               "habitat"),
      Value = as.character(c(
        input$capShape, input$capSurface, input$capColor, input$bruises, 
        input$odor, input$gillAttachment, input$gillSpacing, input$gillSize, 
        input$gillColor, input$stalkShape, input$stalkRoot, 
        input$stalkSurfaceAboveRing, input$stalkSurfaceBelowRing, 
        input$stalkColorAboveRing, input$stalkColorBelowRing, 
        input$veilColor, input$ringNumber, input$ringType, 
        input$sporePrintColor, input$population, input$habitat
      )),
      stringsAsFactors = FALSE
    )
    input <- transpose(df)
    model <- readRDS("model/svm.RDS")
    
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    input <- read.csv(paste("input", ".csv", sep=""), header = TRUE, stringsAsFactors = TRUE)
    
    levels(input$cap.shape) <- levels(msrm$cap.shape)
    levels(input$cap.surface) <- levels(msrm$cap.surface)
    levels(input$cap.color) <- levels(msrm$cap.color)
    
    levels(input$bruises) <- levels(msrm$bruises)
    
    levels(input$odor) <- levels(msrm$odor)
    
    levels(input$gill.attachment) <- levels(msrm$gill.attachment)
    levels(input$gill.spacing) <- levels(msrm$gill.spacing) 
    levels(input$gill.size) <- levels(msrm$gill.size) 
    levels(input$gill.color) <- levels(msrm$gill.color)
    
    levels(input$stalk.shape) <- levels(msrm$stalk.shape)
    levels(input$stalk.root) <- levels(msrm$stalk.root)
    levels(input$stalk.surface.above.ring) <- levels(msrm$stalk.surface.above.ring)
    levels(input$stalk.surface.below.ring) <- levels(msrm$stalk.surface.below.ring)
    levels(input$stalk.color.above.ring) <- levels(msrm$stalk.color.above.ring)
    levels(input$stalk.color.below.ring) <- levels(msrm$stalk.color.below.ring)
    
    levels(input$veil.color) <- levels(msrm$veil.color)
    
    levels(input$ring.number) <- levels(msrm$ring.number)
    levels(input$ring.type) <- levels(msrm$ring.type)
    
    levels(input$spore.print.color) <- levels(msrm$spore.print.color)
    
    
    levels(input$population) <- levels(msrm$population)
    
    levels(input$habitat) <- levels(msrm$habitat)    
    
    return(df)
  }
  
  output$contents <- renderPrint({
    if (input$btnSubmit > 0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  output$textResult <- renderText({
    paste(datasetInput())
  })
  
  output$txtout <- renderText({
    paste(input$capShape, input$capSurface, input$capColor, input$bruises, 
          input$odor, input$gillAttachment, input$gillSpacing, input$gillSize, 
          input$gillColor, input$stalkShape, input$stalkRoot, 
          input$stalkSurfaceAboveRing, input$stalkSurfaceBelowRing, 
          input$stalkColorAboveRing, input$stalkColorBelowRing, 
          input$veilColor, input$ringNumber, input$ringType, 
          input$sporePrintColor, input$population, input$habitat, sep = "\n")
  })
}

shinyApp(ui = ui, server = server)