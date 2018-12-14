#' @title Create salinity duration hydrograph plots for sites
#'
#' @description For each site in the salinity object, plot the salinity duration hydrograph.
#'
#' @param sal data.frame salinity object with daily or finer resolution salinity data stored in daily_data attribute. Created by CSIimport_daily, CSIimport_unit, or CSIimport_NERRS functions.
#' @param dir character Directory to write output files to.
#' @param thumbs logical. If true, thumbnail plots will be generated in addition to full-sized plots.
#' @param leg logical. If true, legend will be displayed in upper left corner.
#'
#' @importFrom grDevices dev.off png
#' @importFrom graphics axis par plot rect text
#'
#' @export
#'
#' @examples
#' # Data file with Year, Month, and Day columns
#' data_path <- system.file("extdata", "Daily_Waccamaw_LittleBlack_Rivers.csv", package="CSI")
#' sal <- CSIimport_monthly(data_path)
#' CSIduration_hydrograph(sal)
#'
CSIduration_hydrograph <- function (sal, dir = paste0(getwd(), "/duration_hydrographs"), thumbs = F, leg = T) {
  if (is.null(attributes(sal)$daily_data))
    stop("sal salinity data object must be daily or finer resolution")
  if (!(length(dir) == 1) || !is.character(dir))
    stop("dir must be a single character string")
  if (!dir.exists(dir)) dir.create(dir)
  col <- c("tan4", "tan2", "darkolivegreen4", "lightblue", "skyblue3", "darkgreen")
  sal <- attributes(sal)$daily_data
  xrange <- seq.Date(as.Date(paste(sal$Year[1], sal$Month[1], sal$Day[1], sep = "-")), as.Date(paste(rev(sal$Year)[1], rev(sal$Month)[1], rev(sal$Day)[1], sep = "-")), by = "day")
  num_sites <- dim(sal)[2] - 3
  for (j in 1:num_sites) {
    por <- data.frame(date = xrange, sal = sal[, j + 3])
    yr3 <- por[(dim(sal)[1] - 365 * 3):dim(sal)[1], ]
    win30 <- rep(NA, length(yr3$date))
    for (i in 30:length(win30))
      win30[i] <- mean(yr3$sal[(i-29):i], na.rm = T)
    qyear <- matrix(NA, length(yr3$date), 6); median <- matrix(NA, length(yr3$date), 1)
    for (i in 1:length(qyear[, 1])) {
      qyear[i, ] <- quantile(por$sal[as.POSIXlt(por$date)$mon == as.numeric(format(yr3$date[i], "%m")) - 1], c(0, .1, .25, .75, .9, 1), na.rm = T)
      median[i, ] <- quantile(por$sal[as.POSIXlt(por$date)$mon == as.numeric(format(yr3$date[i], "%m")) - 1], 0.5, na.rm = T)
    }
    median[as.POSIXlt(yr3$date)$mday == 1] <- NA
    if (thumbs) {
      png(paste0(dir, "/", names(sal)[j + 3], "_salinity_thumb.png"), width = 360, height = 150, pointsize = 2)
      par(mar = c(0, 0, 0, 0) + .1)
      plot(qyear[, 1], type = "n", xlab = "", ylab = "", ylim = range(qyear, na.rm = T), xaxt = "n", yaxt = "n", main = "", axes = F, frame.plot = T)
      for (i in 1:5) polygon(c(1:length(yr3$date), length(yr3$date):1), c(qyear[, i], rev(qyear[, i + 1])), col = col[i])
      lines(median, lwd = 2, col = "yellow")
      lines(yr3$sal, lwd = 3, col = "grey")
      lines(win30, lwd = 4, col = "black")
      dev.off()
    }
    png(paste0(dir, "/", names(sal)[j + 3], "_salinity.png"), width = 2400, height = 800)
    par(mar = c(5, 4, 4, 5) + .1)
    plot(qyear[, 1], type = "n", xlab = "Month of year", ylab = "Salinity (PPT)", ylim = range(qyear, na.rm = T), xaxt = "n", main = paste(names(sal)[j + 3], "three year salinity, 30 day moving window"))
    axis(1, at = which(as.POSIXlt(yr3$date)$mday == 1), labels = format(yr3$date[which(as.POSIXlt(yr3$date)$mday == 1)], "%b"), hadj = -1)
    axis(1, at = which(as.POSIXlt(yr3$date)$yday == 0), labels = format(yr3$date[which(as.POSIXlt(yr3$date)$yday == 0)], "%Y"), padj = 1)
    for (i in 1:5) polygon(c(1:length(yr3$date), length(yr3$date):1), c(qyear[, i], rev(qyear[, i + 1])), col = col[i])
    lines(median, lwd = 3, col = "yellow")
    lines(yr3$sal, lwd = 4, col = "grey")
    lines(win30, lwd = 5, col = "black")
    if (leg) {
      legend("topright", c("90% to Max.", "75% to 90%", "25% to 75%", "10% to 25%", "Min. to 10%"), fill = col[5:1], cex = 1.5, bty = "n", title = "Salinity bins")
      legend("topright", c("Salinity 30 day moving window", "Daily salinity values", "Historic monthly mean salinity"), lwd = c(5, 4, 4), col = c("black", "grey", "yellow"), inset = c(.075, 0), cex = 1.5, bty = "n")
      legend("topleft", paste("Period of record:", xrange[1], "to", rev(xrange)[1]), cex = 1.5, bty = "n")
    }
    dev.off()
  }
}

#' @title Create salinity duration hydrograph plots for sites
#'
#' @description For each site in the salinity object, plot the salinity duration hydrograph.
#'
#' @description Alias of CSIduration_hydrograph.
#'
#' @param sal data.frame salinity object with daily or finer resolution salinity data stored in orig_data attribute. Created by CSIimport_daily, CSIimport_unit, or CSIimport_NERRS functions.
#' @param dir character Directory to write output files to.
#' @param thumbs logical. If true, thumbnail plots will be generated in addition to full-sized plots.
#' @param leg logical. If true, legend will be displayed in upper left corner.
#'
#' @export
#'
CSIdur_hyd <- CSIduration_hydrograph
