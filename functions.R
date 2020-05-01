##########
# Key functions
##########

comboWrapper <- function(elem.subset, total.sum, tol, progress){
  ### Iterate through all possible sums within tolerance
  
  allcombos <<- matrix(nrow=0,ncol = length(elem.subset$BaseElements),dimnames = list(NULL, elem.subset$BaseElements))
  curr.diff <- 0
  n <- length(seq(total.sum-tol, total.sum+tol, by=1))
  for(possiblesums in seq(total.sum-tol, total.sum+tol, by=1)){
    
    getElementCombinations(elem.subset, possiblesums, 1, c(), NULL, curr.diff)
    progress$inc(1/n, detail = paste("Doing part", curr.diff+1, " of ", tol*2+1))
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
