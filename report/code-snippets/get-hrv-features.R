hrv.features <- data.frame()
hrv.feature.names <- c("mean.rr","sdnn","mean.hr","sdhr","rmssd","nn50","pnn50","sdann","sdnni","hrvtri","tinn","vlfpeakfft","lfpeakfft","hfpeakfft","vlfpowfft","lfpowfft","hfpowfft","vlfpowprfft","lfpowprfft","hfpowprfft","lfpownufft","hfpownufft","totpowfft","lfhffft","vlfpeakar","lfpeakar","hfpeakar","vlfpowar","lfpowar","hfpowar","vlfpowprar","lfpowprar","hfpowprar","lfpownuar","hfpownuar","totpowar","lfhfar","edr","sd1","sd2","apen","sampen","d2","dfa1","dfa2")
for (i in 1:nrow(fss.features)) {
  
  session.start <- fss.features[i, 9]
  measurement <- fss.features[i, 14]
  
  hrv.feature.path <- paste(root.path, feature.directory, activity.directory, user.directory, "kubios-hrv-features", "/", strftime(session.start, format="%Y-%m-%d--%H-%M-%S"), "/", ecg.data.file.name, "-", measurement, "_hrv.txt", sep = "")
  rm(measurement)
  
  if(file.exists(hrv.feature.path)) {
    hrv.feature.row <- read.csv(hrv.feature.path, nrows = 1)
    hrv.feature.row <- hrv.feature.row[, 2:46]
    names(hrv.feature.row) <- hrv.feature.names
    hrv.features <- rbind(hrv.features, hrv.feature.row)
  } 
  
  else {
    hrv.feature.row <- data.frame(t(rep(NA, 45)))
    names(hrv.feature.row) <- hrv.feature.names
    hrv.features <- rbind(hrv.features, hrv.feature.row)
  }
  rm(hrv.feature.path, hrv.feature.row)
}
rm(hrv.feature.names, i, session.start)