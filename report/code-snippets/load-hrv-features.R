hrv.features  <- data.frame()
for (i in 1:nrow(fss.features)) {
  source("code-snippets/set-additional-features.R")
  hrv.feature.path <- paste(root.path, "features", "/", activity, "/", user, "/", "kubios-hrv-features", "/", strftime(date, format="%Y-%m-%d--%H-%M-%S"), "/", hrv.file.name.prefix, measurement, "_hrv.txt", sep = "")
  if(file.exists(hrv.feature.path)) {
    hrv.feature.rows <- read.csv(hrv.feature.path, skip = 0)
    hrv.feature.rows <- hrv.feature.rows[, 1:46]
    names(hrv.feature.rows) <- c("sample","meanrr","sdnn","meanhr","sdhr","rmssd","nn50","pnn50","sdann","sdnni","hrvtri","tinn","vlfpeakfft","lfpeakfft","hfpeakfft","vlfpowfft","lfpowfft","hfpowfft","vlfpowprfft","lfpowprfft","hfpowprfft","lfpownufft","hfpownufft","totpowfft","lfhffft","vlfpeakar","lfpeakar","hfpeakar","vlfpowar","lfpowar","hfpowar","vlfpowprar","lfpowprar","hfpowprar","lfpownuar","hfpownuar","totpowar","lfhfar","edr","sd1","sd2","apen","sampen","d2","dfa1","dfa2")
    hrv.features <- rbind(hrv.features, hrv.feature.rows)
    rm(hrv.feature.rows)
  } else {
    for (j in 1:m) {
      hrv.feature.row <- data.frame(t(rep(NA, 46)))
      names(hrv.feature.row) <- c("sample","meanrr","sdnn","meanhr","sdhr","rmssd","nn50","pnn50","sdann","sdnni","hrvtri","tinn","vlfpeakfft","lfpeakfft","hfpeakfft","vlfpowfft","lfpowfft","hfpowfft","vlfpowprfft","lfpowprfft","hfpowprfft","lfpownufft","hfpownufft","totpowfft","lfhffft","vlfpeakar","lfpeakar","hfpeakar","vlfpowar","lfpowar","hfpowar","vlfpowprar","lfpowprar","hfpowprar","lfpownuar","hfpownuar","totpowar","lfhfar","edr","sd1","sd2","apen","sampen","d2","dfa1","dfa2")
      hrv.features <- rbind(hrv.features, hrv.feature.row)
    }
    rm(hrv.feature.row, j)
  }
}
rm(hrv.feature.path, i, additional.features, activity.start, date, measurement, m, hrv.file.name.prefix, jc.file.name.prefix)