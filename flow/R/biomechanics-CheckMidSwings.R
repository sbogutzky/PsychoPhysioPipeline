#' Checks mid swing visually.
#' 
#' \code{CheckMidSwings} return checked mid swing indexes.
#' 
#' @param t.s. t.s. A numerical vector of seconds
#' @param angular.velocity.deg.s. A numerical vector of angular velocity in deg/s
#' @param mid.swing.indexes. A numerical vector of mid swing indexes
#' @return A numerical vector of checked mid swing indexes

CheckMidSwings <- function(t.s, angular.velocity.deg.s, mid.swing.indexes) {
  
  t.s.subset <- t.s[mid.swing.indexes]
  angular.velocity.deg.s.subset <- angular.velocity.deg.s[mid.swing.indexes]
  cycle.interval.t.s <- diff(t.s.subset)
  cycle.interval.t.s <- c(mean(cycle.interval.t.s), cycle.interval.t.s)
  
  InternalPlotFunction <- function(t.s, angular.velocity.deg.s, t.s.subset, angular.velocity.deg.s.subset, cycle.interval.t.s) {
    par(mfcol = c(2, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
    
    plot(t.s, angular.velocity.deg.s, type = "l", xlab = "Time (s)", ylab = expression("Angular Velocity ("~deg/s~")"))
    points(t.s.subset, angular.velocity.deg.s.subset, pch = 21, bg = "red")
    
    plot(t.s.subset, cycle.interval.t.s, xlab = "Time (s)", ylab = "Cycle Interval (s)")
  }
  
  InternalPlotFunction(t.s, angular.velocity.deg.s, t.s.subset, angular.velocity.deg.s.subset, cycle.interval.t.s)
  
  mean.cycle.interval.s <- mean(cycle.interval.t.s)
  sd.cycle.interval.s <- sd(cycle.interval.t.s)
  print(paste("Mean cycle interval: ", round(mean.cycle.interval.s, 2), "s", sep = ""))
  print(paste("SD cycle interval: ", round(sd.cycle.interval.s, 2), "s", sep = ""))
  
  answer <- readline("Are you finish? Type Y and press return > ")
  i <- 1
  while(answer != "Y" & length(t.s.subset[cycle.interval.t.s < mean.cycle.interval.s - 5 * sd.cycle.interval.s | cycle.interval.t.s > mean.cycle.interval.s + 5 * sd.cycle.interval.s]) >= i) {
    outlier <- round(t.s.subset[cycle.interval.t.s < mean.cycle.interval.s - 5 * sd.cycle.interval.s | cycle.interval.t.s > mean.cycle.interval.s + 5 * sd.cycle.interval.s])[i]
    range.s <- c(outlier - 15, outlier + 15)
    
    par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
    in.line <- t.s >= range.s[1] & t.s < range.s[2]
    in.points <- t.s.subset >= range.s[1] & t.s.subset < range.s[2]
    
    plot(t.s[in.line], angular.velocity.deg.s[in.line], type = "l", xlab = "Time (s)", ylab = expression("Angular Velocity ("~deg/s~")"))
    points(t.s.subset[in.points], angular.velocity.deg.s.subset[in.points], pch = 21, bg = "red")
    title("Select to add")
    
    # Add mid swings and control
    add  <- identify(t.s, angular.velocity.deg.s)
    if(length(add) > 0) {
      mid.swing.indexes <- c(mid.swing.indexes, add)
      mid.swing.indexes <- sort(mid.swing.indexes)
      t.s.subset <- t.s[mid.swing.indexes]
      angular.velocity.deg.s.subset <- angular.velocity.deg.s[mid.swing.indexes]
      cycle.interval.t.s <- diff(t.s.subset)
      cycle.interval.t.s <- c(mean(cycle.interval.t.s), cycle.interval.t.s)
      
      mean.cycle.interval.s <- mean(cycle.interval.t.s)
      sd.cycle.interval.s <- sd(cycle.interval.t.s)
      print(paste("Mean cycle interval: ", round(mean.cycle.interval.s, 2), "s", sep = ""))
      print(paste("SD cycle interval: ", round(sd.cycle.interval.s, 2), "s", sep = ""))
    } else {
      i <- i + 1
    }
    
    answer <- readline("Are you finish (add)? Type Y and press return > ")
  }
  
  InternalPlotFunction(t.s, angular.velocity.deg.s, t.s.subset, angular.velocity.deg.s.subset, cycle.interval.t.s)
  
  mean.cycle.interval.s <- mean(cycle.interval.t.s)
  sd.cycle.interval.s <- sd(cycle.interval.t.s)
  print(paste("Mean cycle interval: ", round(mean.cycle.interval.s, 2), "s", sep = ""))
  print(paste("SD cycle interval: ", round(sd.cycle.interval.s, 2), "s", sep = ""))
  
  answer <- readline("Are you finish? Type Y and press return > ")
  i <- 1
  while(answer != "Y" & length(t.s.subset[cycle.interval.t.s < mean.cycle.interval.s - 5 * sd.cycle.interval.s | cycle.interval.t.s > mean.cycle.interval.s + 5 * sd.cycle.interval.s]) >= i) {
    outlier <- round(t.s.subset[cycle.interval.t.s < mean.cycle.interval.s - 5 * sd.cycle.interval.s | cycle.interval.t.s > mean.cycle.interval.s + 5 * sd.cycle.interval.s])[i]
    range.s <- c(outlier - 15, outlier + 15)
    
    par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
    in.line <- t.s >= range.s[1] & t.s < range.s[2]
    in.points <- t.s.subset >= range.s[1] & t.s.subset < range.s[2]
    
    plot(t.s[in.line], angular.velocity.deg.s[in.line], type = "l", xlab = "Time (s)", ylab = expression("Angular Velocity ("~deg/s~")"))
    points(t.s.subset[in.points], angular.velocity.deg.s.subset[in.points], pch = 21, bg = "red")
    title("Select to remove")
    
    remove  <- identify(t.s.subset, angular.velocity.deg.s.subset)
    
    # Remove selected mid swings
    if(length(remove) > 0) {
      mid.swing.indexes <- mid.swing.indexes[-remove]
      t.s.subset <- t.s[mid.swing.indexes]
      angular.velocity.deg.s.subset <- angular.velocity.deg.s[mid.swing.indexes]
      cycle.interval.t.s <- diff(t.s.subset)
      cycle.interval.t.s <- c(mean(cycle.interval.t.s), cycle.interval.t.s)
      
      mean.cycle.interval.s <- mean(cycle.interval.t.s)
      sd.cycle.interval.s <- sd(cycle.interval.t.s)
      print(paste("Mean cycle interval: ", round(mean.cycle.interval.s, 2), "s", sep = ""))
      print(paste("SD cycle interval: ", round(sd.cycle.interval.s, 2), "s", sep = ""))
    } else {
      i <- i + 1
    }
    
    answer <- readline("Are you finish (remove)? Type Y and press return > ")
  }
  
  InternalPlotFunction(t.s, angular.velocity.deg.s, t.s.subset, angular.velocity.deg.s.subset, cycle.interval.t.s)

  return(mid.swing.indexes)
}