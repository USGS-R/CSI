#' @title Create stacked CSI interval plots for sites
#'
#' @description For each site (layer) in the csi object, plot the intervals (columns) on a stacked plot.
#'
#' @param csi array A 3D array of CSI values with dimensions of number of months covered, scale of months analysed (typically 1-24), and number of sites.
#' @param dir character Directory to write output files to.
#' @param thumbs logical. If true, thumbnail plots will be generated in addition to full-sized plots.
#'
#' @importFrom grDevices dev.off png
#' @importFrom graphics axis par plot rect
#'
#' @export
#'
#' @examples
#' # Data file with Year and Month columns
#' data_path <- system.file("extdata", "Monthly_Coastal_EDEN.csv", package = "CSI")
#' sal <- CSIimport_monthly(data_path)
#' csi <- CSIcalc(sal)
#' CSIstack(csi)
#'
CSIstack <- function (csi, dir = paste0(getwd(), "/csi_stacked"), thumbs = F) {
  if (!(length(dir) == 1) || !is.character(dir))
    stop("dir must be a single character string")
  if (!dir.exists(dir)) dir.create(dir)
  csi.breaks <- c(-1000, -2, -1.6, -1.3, -0.8, -0.5, 0.5, 0.8, 1.3, 1.6, 2, 1000)
  csi.cols <- c("#740200CC", "#E80600CC", "#FFAC00CC", "#FCD57BCC", "#FEFF00CC", "#FDFFE4CC", "#E4E1EACC", "#B6D0E5CC", "#91ABD0CC", "#33648DCC", "#23425FCC")
  yrange <- seq.Date(as.Date(paste(dimnames(csi)[[1]][1], "01", sep = "-")), as.Date(paste(rev(dimnames(csi)[[1]])[1], "01" , sep = "-")), by = "month")
  yrange2 <- yrange + 15 # Offset to plot midmonth
  sal <- attr(csi, "sal")
  max <- ceiling(max(sal[, 3:dim(sal)[2]], na.rm = T))
  num_sites <- dim(csi)[3]
  num_months <- dim(csi)[1]
  scale <- dim(csi)[2]
  int_ht <- max / scale # Height of scale interval for stacked plot
  for (j in 1:num_sites) {
    mwa <- NULL # 12-month moving-window average
    for (k in 1:(num_months - 11))
      mwa[k] <- mean(sal[k:(k + 11), j + 2])
    mwa <- c(rep(NA, num_months - length(mwa)), mwa) # pad
    if (thumbs) {
      png(filename = paste0(dir, "/", dimnames(csi)[[3]][j], "_stacked_thumb.png"), width = 360, height = 150, units = "px", pointsize = 2, type = "quartz")
      par(mar = c(0, 0, 0, 0) + .1)
      plot(yrange, sal[, j + 2], type = "n", ylim = c(0, max), ylab = "", xlab = "", main = "", xaxt = "n", yaxt = "n", axes = F, frame.plot = T)
      for (i in 1:24) {
        bin <- cut(unlist(csi[, i, j]), csi.breaks, labels = F)
        for(k in 1:num_months) if(!is.na(bin[k])) rect(as.numeric(yrange[k]), i * int_ht - int_ht, as.numeric(yrange[k + 1]),i * int_ht, col = csi.cols[bin[k]], border = NA)
      }
      lines(yrange2, mwa, lwd = 2)
      dev.off()
    }
    png(filename = paste0(dir, "/", dimnames(csi)[[3]][j], "_stacked.png"), width = 1724, height = 614, units = "px", pointsize = 12, type = "quartz")
    par(mar = c(5.1, 4.1, 4.1, 4.1))
    plot(yrange, sal[, j + 2], type = "n", ylim = c(0, max), ylab = "Coastal Salinity Index interval (months)", xlab = "Date", main = paste0(dimnames(csi)[[3]][j], " Coastal Salinity Index with 1-", scale, " month interval"), axes = F, frame.plot = T)
    for (i in 1:scale) {
      bin <- cut(unlist(csi[, i, j]), csi.breaks, labels = F)
      for (k in 1:num_months) if (!is.na(bin[k])) rect(as.numeric(yrange[k]), i * int_ht - int_ht, as.numeric(yrange[k + 1]), i * int_ht, col = csi.cols[bin[k]], border = NA)
    }
    lines(yrange2, mwa, lwd = 3, col = "lightskyblue")
    abline(h = mean(sal[, j + 2], na.rm = T), lwd = 3, col = "darkgrey")
    abline(h = quantile(sal[, j + 2], c(.25, .75), na.rm = T), lwd = 3,col = "lightgrey")
    axis(1, as.numeric(seq.Date(as.Date(paste0(sal$Year[1], "/1/1")), as.Date(paste0(sal$Year[num_months], "/1/1")), by = "year")), sal$Year[1]:sal$Year[num_months])
    axis(2, seq(int_ht / 2, max, int_ht), 1:scale)
    axis(4, c(0, 5, 18, 30, 40, 50), lwd.ticks = 3)
    rect(as.numeric(yrange[num_months] + 150), -50, as.numeric(yrange[num_months]) +300, 5.25, col = "cyan1", border = NA)
    rect(as.numeric(yrange[num_months] + 150), 5.25, as.numeric(yrange[num_months]) +300, 18, col = "cyan2", border = NA)
    rect(as.numeric(yrange[num_months] + 150), 18, as.numeric(yrange[num_months]) +300, 30, col = "cyan3", border = NA)
    rect(as.numeric(yrange[num_months] + 150), 30, as.numeric(yrange[num_months]) +300, 40, col = "cyan4", border = NA)
    rect(as.numeric(yrange[num_months] + 150), 40, as.numeric(yrange[num_months]) +300, 80, col = "deepskyblue4", border = NA)
    axis(4, c(1.5, 11.5, 24, 35, 49), c("Oligohaline", "Mesohaline", "Polyhaline", "Euhaline", "Hypersaline"), tick = F, padj = -4.5, font = 4, cex.axis = 0.9)
    mtext("Period of record values and estuarine salinity ranges, in practical salinity units", 4, 3)
    fst <- which(!is.na(sal[, j + 2]))[1]
    lst <- tail(which(!is.na(sal[, j + 2])), 1)
    mtext(paste0("Period of record: ", sal$Month[fst], "/", sal$Year[fst], " - ", sal$Month[lst], "/", sal$Year[lst]), 3, adj = 0)
    legend("topleft", c("12-month rolling average", "Mean", "25th and 75th Percentile"), lwd = 3, col=c("lightskyblue", "darkgrey", "lightgrey"))
    legend("topleft", c("CD4", "CD3", "CD2", "CD1", "CD0", "Normal", "CW0", "CW1", "CW2", "CW3", "CW4"), fill = csi.cols, inset = c(0, 0.125))
    dev.off()
  }
}
