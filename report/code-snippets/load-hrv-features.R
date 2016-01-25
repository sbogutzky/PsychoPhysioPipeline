hrv.features <- data.frame()
for (i in 1:nrow(fss.features)) {
  
  source("code-snippets/set-additional-features.R")
  
  # Create HRV feature data frame
  directory.path  <- paste(root.data.directory.path, "features/", activity.directory, user.directory, "kubios-hrv-features/", date.directory, sep="")
  file.name <- paste("ecg-data-", measurement, "_hrv.txt", sep = "")
  col.names <- c("sample","meanrr","sdnn","meanhr","sdhr","rmssd","nn50","pnn50","sdann","sdnni","hrvtri","tinn","vlfpeakfft","lfpeakfft","hfpeakfft","vlfpowfft","lfpowfft","hfpowfft","vlfpowprfft","lfpowprfft","hfpowprfft","lfpownufft","hfpownufft","totpowfft","lfhffft","vlfpeakar","lfpeakar","hfpeakar","vlfpowar","lfpowar","hfpowar","vlfpowprar","lfpowprar","hfpowprar","lfpownuar","hfpownuar","totpowar","lfhfar","edr","sd1","sd2","apen","sampen","d2","dfa1","dfa2","activity","activity.start","activity.end","inquiry.end","measurement","last.name","first.name","date.of.birth")
  feature.data.frame <- CreateFeatureDataFrame(directory.path, file.name, 1:46, 1:3, col.names, additional.features, skip = 0)
  hrv.features <- rbind(hrv.features, feature.data.frame)
}

rm(additional.features, directory.path, feature.data.frame, file.name, activity.start, col.names, i, measurement)
