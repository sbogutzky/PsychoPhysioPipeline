library(dygraphs)
library(xts)
library(htmlwidgets)

axisLabelFormatter <- "function (ms) {var d = new Date(ms); return Dygraph.zeropad(d.getHours()) + ':' + Dygraph.zeropad(d.getMinutes()) + ':' + Dygraph.zeropad(d.getSeconds()) + '.' + Dygraph.zeropad(d.getMilliseconds());}"
valueFormatter <- "function (ms) { var d = new Date(ms); return Dygraph.zeropad(d.getHours()) + ':' + Dygraph.zeropad(d.getMinutes()) + ':' + Dygraph.zeropad(d.getSeconds()) + '.' + Dygraph.zeropad(d.getMilliseconds());}"

data.xts <- xts(lead.1.mv, as.POSIXlt(first.timestamp / 1000, tz="CET", origin="1970-01-01") + t.ms)
dygraph(data.xts, main = strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y/%m/%d %H:%M"), ylab = "Lead I [mV]") %>% 
  dyAxis("x",valueFormatter=JS(valueFormatter), axisLabelFormatter=JS(axisLabelFormatter)) %>% 
  dySeries("V1", label = "Lead I [mV]") %>%
  dyRangeSelector()