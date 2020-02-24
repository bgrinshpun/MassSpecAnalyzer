#########################################################
## Rshiny application to analyze mass spectrometry data
## for possible combinations of masses with tolerance range.
##
## Author: Boris Grinshpun
## 2019
##
#########################################################


setwd('path/to/files')  # set to location of input files
options(stringsAsFactors = F)


##########
# Check for Required packages
##########
for (package in c('shiny', 'shinyjs', 'openxlsx')) {
  if (!require(package, character.only=T, quietly=T)) {
      install.packages(package)
      library(package, character.only=T)
  }
}

##########
# Load data
##########
defaults <- read.table('defaults.csv',header=F, sep=',')
inputfile <- defaults$V2[defaults$V1=='elementsfile']
elementfile <- read.table(inputfile, header=T, sep=',')
elementfile=elementfile[order(elementfile$MW, decreasing = T),]


##########
# Preprocessing
##########
defaultSum <- as.numeric(defaults$V2[defaults$V1=='sum'])
defaultTolerance <- as.numeric(defaults$V2[defaults$V1=='tolerance'])

BaseElements <- elementfile$BaseElements
MW <-  elementfile$MW
Minimums <- elementfile$Minimums
Maximums <- elementfile$Maximums

m.min <- Minimums
m.min[is.na(m.min)] <- ""
for(i in seq(m.min)){
  if(m.min[i] != ""){
    m.min[i] <- paste('; min = ',m.min[i], sep='')
  }
}
m.max <- Maximums
m.max[is.na(m.max)] <- ""
for(i in seq(m.max)){
  if(m.max[i] != ""){
    m.max[i] <- paste('; max = ',m.max[i], sep='')
  }
}

m.restrictions=paste(m.min, m.max, sep='')

##########
# Algorithm Function Goes Here
##########

comboWrapper <- function(elem.subset, total.sum, tol, progress){
  ### Iterate through all possible sums within tolerance
  
    allcombos <<- matrix(nrow=0,ncol = length(elem.subset$BaseElements),dimnames = list(NULL, elem.subset$BaseElements))
    curr.diff <- 0
    n <- length(seq(total.sum-tol, total.sum+tol, by=1))
    for(possiblesums in seq(total.sum-tol, total.sum+tol, by=1)){

       getElementCombinations(elem.subset, possiblesums, 1, c(), NULL, curr.diff)
       progress$inc(1/n, detail = paste("Doing part", curr.diff+1))
       curr.diff <- curr.diff+1
    }
    return(allcombos)
}
getElementCombinations <- function(elem.subset, mass.remaining, i, curr.combo, j, min.tol){
  ### Determine combinations for a particular sum
    curr.combo <- c(curr.combo, j)
    cur.elem.mass <- elem.subset$MW[i]
    
    if(mass.remaining==0 | i==length(elem.subset$BaseElements)){
      if(i==length(elem.subset$BaseElements) & mass.remaining>0){
        if(mass.remaining%%cur.elem.mass==0){
          curr.combo <- c(curr.combo,mass.remaining/cur.elem.mass)
          i <- i + 1
        }
        else if(mass.remaining%%cur.elem.mass <= min.tol){
          curr.combo <- c(curr.combo,floor(mass.remaining/cur.elem.mass))
          i <- i + 1
          
        } 
        else {return(1)}
      }
      while( i < length(elem.subset$BaseElements)+1){
        curr.combo <- c(curr.combo, 0)
        i <- i + 1
      }

      allcombos <<- rbind(allcombos, curr.combo)

      return(1)
    }
    
    maxdivisions <- as.integer(mass.remaining/cur.elem.mass)
    # for(j in seq(0,maxdivisions)){
    if(mass.remaining>0){
      for(j in seq(max(0, elem.subset$Minimums[i],na.rm=TRUE),min(elem.subset$Maximums[i], maxdivisions,na.rm=TRUE))){
        getElementCombinations(elem.subset,mass.remaining-cur.elem.mass*j, i+1, curr.combo, j, min.tol)
      }
    }
  }
  

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

##########
# Rendering
##########
server <- function(input, output, session) {
  
  vals <- reactiveValues()
  vals$result <- list()

  ###### Run the calculations
  observeEvent(input$evaluate , {
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Computing result", value = 0)
    start_time <- Sys.time()
    elementfile$BaseElements %in% input$elements
    elem.subset <- elementfile[elementfile$BaseElements %in% input$elements,]
    total.sum <- input$totalsum
    tol <- input$tolerance
    
    allcombos <- comboWrapper(elem.subset,  total.sum, tol, progress)
    
    
    
  ###### Remove solutions that fail restrictions on number of times used
    minimum.inds <- which(!is.na(elem.subset$Minimums))
    maximum.inds <- which(!is.na(elem.subset$Maximums))
    
    for(i in seq(length(maximum.inds))){
      if(nrow(allcombos)==1){
         allcombos <- allcombos[allcombos[maximum.inds[i]]<=elem.subset$Maximums[maximum.inds[i]]]  
      }
      else if(nrow(allcombos)>1){
         allcombos <- allcombos[allcombos[,maximum.inds[i]]<=elem.subset$Maximums[maximum.inds[i]],]  
      }
    }
    
    for(i in seq(length(minimum.inds))){
      if(nrow(allcombos)==1){
         allcombos <- allcombos[allcombos[minimum.inds[i]]>=elem.subset$Minimums[minimum.inds[i]]]  
      }
      else if(nrow(allcombos)>1){
         allcombos <- allcombos[allcombos[,minimum.inds[i]]>=elem.subset$Minimums[minimum.inds[i]],]  
      }
    }
    
    allcombos <- unique(allcombos)
    
    if(nrow(allcombos)>0){
      allcombos <- cbind(allcombos,rowSums(allcombos %*% diag(elem.subset$MW)))
      dimnames(allcombos)[[2]] <- c(paste(elem.subset$BaseElements, ' (',i, elem.subset$MW ,'amu)', sep=""), "Sum") # adjust names
      allcombos <- allcombos[order(allcombos[, 'Sum']),]
    }
    
    
    end_time <- Sys.time()
    total.time <- end_time - start_time  
    
    
    if(is.null(nrow(allcombos)) | nrow(allcombos)==0){
         # Add no result screen
      output$completion_message <- renderText({ 
        paste("Computation finished in ", signif(total.time,3), " seconds. <br /> <br /> NO VALID COMBINATIONS WERE FOUND. PLEASE CHANGE SEARCH PARAMETERS.", sep='')
      })
    }
    else{
    ######  Write to file
      result=paste(input$outputname,'.xlsx', sep='')
      write.xlsx(allcombos, file=result, sheetName = "Results", 
               col.names = TRUE, row.names = FALSE, append = FALSE)
      output$completion_message <- renderText({ 
        paste("Computation finished in ", signif(total.time,3), ' seconds. <br /> <br /> Results saved to ',result, sep='')
      })
    }
  })
  
}

shinyApp(ui = ui, server=server)