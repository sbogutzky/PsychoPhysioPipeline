# Check reability
library(multilevel)
if(nrow(fss.measurements) > 1) {
  fss.item.statements <- c("Ich fühle mich optimal beansprucht.", "Meine Gedanken bzw. Aktivitäten laufen flüssig und glatt.", "Ich merke gar nicht, wie die Zeit vergeht.", "Ich habe keine Mühe mich zu konzentrieren.", "Mein Kopf ist völlig klar.", "Ich bin ganz vertieft in das, was ich gerade mache.", "Die richtigen Gedanken/ Bewegungen kommen wie von selbst.", "Ich weiß bei jedem Schritt, was ich zu tun habe.", "Ich habe das Gefühl, den Ablauf unter Kontrolle zu haben.", "Ich bin völlig selbstvergessen.")
  fss.item.mean.values <- colMeans(fss.measurements[, 1:10], na.rm = TRUE)
  fss.item.sd.values <- apply(fss.measurements[, 1:10], 2, sd, na.rm = TRUE)
  
  print("---")
  print(paste("Cronbachs Alpha Generalfaktor:", round(multilevel::cronbach(fss.measurements[, 1:10])$Alpha, 2)))
  factor.item.correlation.flow <- cor(fss.features$flow, fss.measurements[, 1:10], use = "complete.obs")[1,]
  data.table.flow <- data.frame(M = round(c(fss.item.mean.values, mean(fss.item.mean.values)), 2), SD = round(c(fss.item.sd.values, mean(fss.item.sd.values)), 2), Trennschaerfe = round(c(factor.item.correlation.flow, NA), 2))
  row.names(data.table.flow) <- c(fss.item.statements, "Gesamtmittelwerte")
  print(data.table.flow)
  
  print("---")
  print(paste("Cronbachs Alpha 'Glatter Verlauf':", round(multilevel::cronbach(fss.measurements[, c(8,7,9,4,5,2)])$Alpha, 2)))
  factor.item.correlation.fluency <- cor(fss.features$fluency, fss.measurements[, c(8,7,9,4,5,2)], use = "complete.obs")[1,]
  data.table.fluency <- data.frame(M = round(c(fss.item.mean.values[c(8,7,9,4,5,2)], mean(fss.item.mean.values[c(8,7,9,4,5,2)])), 2), SD = round(c(fss.item.sd.values[c(8,7,9,4,5,2)], mean(fss.item.sd.values[c(8,7,9,4,5,2)])), 2), Trennschaerfe = round(c(factor.item.correlation.fluency, NA), 2))
  row.names(data.table.fluency) <- c(fss.item.statements[c(8,7,9,4,5,2)], "Gesamtmittelwerte")
  print(data.table.fluency)
  
  print("---")
  print(paste("Cronbachs Alpha 'Absorbiertheit':", round(multilevel::cronbach(fss.measurements[, c(6,1,10,3)])$Alpha, 2)))
  factor.item.correlation.absorption <- cor(fss.features$absorption, fss.measurements[, c(6,1,10,3)], use = "complete.obs")[1,]
  data.table.absorption <- data.frame(M = round(c(fss.item.mean.values[c(6,1,10,3)], mean(fss.item.mean.values[c(6,1,10,3)])), 2), SD = round(c(fss.item.sd.values[c(6,1,10,3)], mean(fss.item.sd.values[c(6,1,10,3)])), 2), Trennschaerfe = round(c(factor.item.correlation.absorption, NA), 2))
  row.names(data.table.absorption) <- c(fss.item.statements[c(6,1,10,3)], "Gesamtmittelwerte")
  print(data.table.absorption)
  
  # Clean up
  rm(factor.item.correlation.absorption, factor.item.correlation.fluency, factor.item.correlation.flow, fss.item.sd.values, fss.item.statements, fss.item.mean.values)
}