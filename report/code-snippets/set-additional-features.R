
# Set additional features
additional.features <- fss.features[i, c(9:17)]
session.start <- additional.features[, 1]
measurement <- additional.features[, 6]
if(measurement == 1) {
    date <- session.start
}
rm(additional.features, session.start)