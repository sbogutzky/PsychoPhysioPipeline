#' Checks mid swing visually.
#' 
#' \code{CheckMidSwings} return checked mid swing indexes.
#' 
#' @param t.ms. t.s. A numerical vector of seconds
#' @param angular.velocity.deg.s. A numerical vector of angular velocity in deg/s
#' @param length.s. A numerical that specifies the length in seconds for the visual control
#' @param mid.swing.indexes. A numerical vector of mid swing indexes
#' @return A numerical vector of checked mid swing indexes

CheckMidSwings <- function(t.ms, angular.velocity.deg.s, length.s, mid.swing.indexes) {
  
  par(mfcol = c(2, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
  plot(t.ms / 1000, angular.velocity.deg.s, type = "l", xlab = "Time (s)", ylab = expression("Angular Velocity ("~deg/s~")"))
  points(t.ms[mid.swing.indexes] / 1000, angular.velocity.deg.s[mid.swing.indexes], pch = 21, bg = "red")
  cycle.interval.t.ms <- diff(t.ms[mid.swing.indexes])
  plot(t.ms[mid.swing.indexes] / 1000, c(mean(cycle.interval.t.ms), cycle.interval.t.ms) / 1000, xlab = "Time (s)", ylab = "Cycle Interval (s)")
  
  answer <- readline("Are you finish? Type Y and press return > ")
  if(answer != "Y") {
    answer <- "N"
    current.t.ms <- t.ms[1]
  }
  
  while(answer == "N") {
    while(current.t.ms < max(t.ms)) {
      
      par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
      in.loop.line <- t.ms >= current.t.ms & t.ms < current.t.ms + length.s * 1000
      in.loop.points <- t.ms[mid.swing.indexes] >= current.t.ms & t.ms[mid.swing.indexes] < current.t.ms + length.s * 1000
      
      plot(t.ms[in.loop.line] / 1000, angular.velocity.deg.s[in.loop.line], type = "l", xlab = "Time (s)", ylab = expression("Angular Velocity ("~deg/s~")"))
      points(t.ms[mid.swing.indexes][in.loop.points] / 1000, angular.velocity.deg.s[mid.swing.indexes][in.loop.points], pch = 21, bg = "red")
      title("Select to remove")
      
      remove  <- identify(t.ms[mid.swing.indexes] / 1000, angular.velocity.deg.s[mid.swing.indexes], tolerance = .5)
      
      # Remove selected mid swings
      if(length(remove) > 0) {
        mid.swing.indexes <- mid.swing.indexes[-remove]
      }
      
      in.loop.line <- t.ms >= current.t.ms & t.ms < current.t.ms + length.s * 1000
      in.loop.points <- t.ms[mid.swing.indexes] >= current.t.ms & t.ms[mid.swing.indexes] < current.t.ms + length.s * 1000
      
      plot(t.ms[in.loop.line] / 1000, angular.velocity.deg.s[in.loop.line], type = "l", xlab = "Time (s)", ylab = expression("Angular Velocity ("~deg/s~")"))
      points(t.ms[mid.swing.indexes][in.loop.points] / 1000, angular.velocity.deg.s[mid.swing.indexes][in.loop.points], pch = 21, bg = "red")
      title("Select to add")
      
      # Add mid swings and control
      add  <- identify(t.ms / 1000, angular.velocity.deg.s, tolerance = .5)
      if(length(add) > 0) {
        mid.swing.indexes <- c(mid.swing.indexes, add)
        mid.swing.indexes <- sort(mid.swing.indexes)
        
        in.loop.line <- t.ms >= current.t.ms & t.ms < current.t.ms + length.s * 1000
        in.loop.points <- t.ms[mid.swing.indexes] >= current.t.ms & t.ms[mid.swing.indexes] < current.t.ms + length.s * 1000
        
        plot(t.ms[in.loop.line] / 1000, angular.velocity.deg.s[in.loop.line], type = "l", xlab = "Time (s)", ylab = expression("Angular Velocity ("~deg/s~")"))
        points(t.ms[mid.swing.indexes][in.loop.points] / 1000, angular.velocity.deg.s[mid.swing.indexes][in.loop.points], pch = 21, bg = "red")
        
        readline("Press return to continue > ")
      }
      
      current.t.ms       <- current.t.ms + length.s * 1000
    }
    
    par(mfcol = c(2, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
    plot(t.ms / 1000, angular.velocity.deg.s, type = "l", xlab = "Time (s)", ylab = expression("Angular Velocity ("~deg/s~")"))
    points(t.ms[mid.swing.indexes] / 1000, angular.velocity.deg.s[mid.swing.indexes], pch = 21, bg = "red")
    cycle.interval.t.ms <- diff(t.ms[mid.swing.indexes])
    plot(t.ms[mid.swing.indexes] / 1000, c(mean(cycle.interval.t.ms), cycle.interval.t.ms) / 1000, xlab = "Time (s)", ylab = expression("Cycle Interval ("~s~")"))
    
    answer <- readline("Are you finish? Type Y and press return > ")
    if(answer != "Y") {
      answer <- "N"
      current.t.ms <- t.ms[1]
    }
  }
  
  return(mid.swing.indexes)
}