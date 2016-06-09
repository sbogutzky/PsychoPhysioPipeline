# Extract times
if(i == 1) {
  activity.start.ms <- 0
} else {
  activity.start.ms <- self.report.end.ms
}

activity.end.ms <- self.report.data$timestamp.start.ms[i]
self.report.end.ms <- self.report.data$timestamp.stop.ms[i]