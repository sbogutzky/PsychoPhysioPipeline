attach(motion.data)
x <- Timestamp/1000
y <- Gyroscope.X
plot(x, y)

# subset
while(T) {
  print("1. Select 2 points for subsetting \n 2. Select 0 points for quitting")
  bounds <- identify(x, y, labels = row.names(motion.data))
  print(bounds)
  if(length(bounds) == 2) {
    plot(x, y, xlim = c(x[min(bounds)], x[max(bounds)]))
    last.bounds <- bounds
  } 
  if (length(bounds) == 0) {
    x <- x[min(last.bounds):max(last.bounds)]
    y <- y[min(last.bounds):max(last.bounds)]
    break
  }
}
