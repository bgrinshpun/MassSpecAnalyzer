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
      nrow(allcombos)
      write.xlsx(allcombos, file=result, sheetName = "Results", 
                 col.names = TRUE, row.names = FALSE, append = FALSE)
      output$completion_message <- renderText({ 
        paste("Computation finished in ", signif(total.time,3), ' seconds. <br /> <br />', 
                      nrow(allcombos),' combinations found! <br /><br /><br />  Results saved to ',result, sep='')
      })
    }
  })
  
}