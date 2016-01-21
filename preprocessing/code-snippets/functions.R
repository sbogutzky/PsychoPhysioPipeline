ComputeCycleJerkCosts <- function(A, mid.swing.indexes) {
  jerk.costs <- c()
  for(i in 1:(length(mid.swing.indexes) - 1)) {
    
    m <- mid.swing.indexes[i]
    n <- mid.swing.indexes[(i+1)]
    # Compute jerk cost of each cycle
    t.ms                <- A[, 1][m:n]
    # acceleration.x.ms.2 <- A[, 2][m:n]
    acceleration.y.ms.2 <- A[, 3][m:n]
    acceleration.z.ms.2 <- A[, 4][m:n]
    
    jerk.cost   <- CalculateJerkCost(t.ms / 1000, data.frame(acceleration.y.ms.2, acceleration.z.ms.2), normalized = T)
    jerk.costs  <- c(jerk.costs, jerk.cost)
  }
  
  return(jerk.costs)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

ComputeCycleMeanMatrix <- function(M, col, mid.swing.indexes) {
  center <- Mode(diff(mid.swing.indexes))
  values <- c()
  j = 0
  for(i in 1:(length(mid.swing.indexes) - 1)) {
    m <- mid.swing.indexes[i]
    n <- mid.swing.indexes[(i+1)]
    values.temp <- M[, col][m:n]
    if(center == length(values.temp)) {
      values <- c(values, values.temp)
      j = j + 1
    }
  }
  M <- matrix(values, center, j)
  
  return(M)
}

saveData <- function(output.data, file.name, preprocessed.data.directory.path, activity.directory, user.directory, date.directory, activity.start) {
  
  # Create directory, if needed
  output.directory.path <- paste(processed.data.directory.path, activity.directory, user.directory, date.directory, sep="")
  if(!file.exists(substr(output.directory.path, 1, nchar(output.directory.path) - 1))) {
    dir.create(output.directory.path, recursive = TRUE)
  }
  
  # Write csv file
  output.file.path <- paste(output.directory.path, file.name, sep = "")
  op <- options(digits.secs=3)
  con <- file(output.file.path, 'w') 
  writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d"), con = con)
  writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%H:%M:%OS"), con = con)
  write.csv(output.data, file = con, row.names = FALSE)
  close(con)
  options(op) #reset options
  print(paste("Wrote:", output.file.path))
}