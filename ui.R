##########
# Layout
##########
choiceNames=paste(BaseElements, ' [MW = ', MW, ' amu' ,m.restrictions,']' , sep='')
ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Mass Spectrometry Analysis Tool"),
  sidebarLayout(
    
    sidebarPanel(
      checkboxGroupInput(inputId = "elements", 
                         label="Select Elements",choiceValues=BaseElements, choiceNames=choiceNames,
                         selected=BaseElements), width=4,
      sliderInput(inputId="totalsum","Total sum of elements",min = 0,max = defaultSum*2,value = defaultSum),
      sliderInput(inputId="tolerance","Tolerance",min = 0,max = defaultTolerance*2 ,value = defaultTolerance),
      textInput(inputId="outputname", "Output Filename", value = "output", width = NULL, placeholder = NULL),
      actionButton(inputId="evaluate", label = "Evaluate Results", icon("frog"))),
    mainPanel(
      hr(),
      tags$h4(htmlOutput("progress_message")),
      br(),
      tags$h4(htmlOutput("completion_message")),
      br(),
      hr()
    )
  )
  
)