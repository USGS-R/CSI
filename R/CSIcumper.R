#' @title Create stacked CSI interval plots for sites
#'
#' @description For each site (layer) in the csi object, plot the intervals (columns) on a stacked plot.
#'
#' @param csi array A 3D array of CSI values with dimensions of number of months covered, scale of months analysed (typically 1-24), and number of sites.
#' @param dir character Directory to write output files to.
#' @param thumbs logical. If true, thumbnail plots will be generated in addition to full-sized plots.
#' @param grouped logical. If true, second y-asix (salinity) will be the same across all output plots.
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
CSIcumper <- function (csi, dir = paste0(getwd(), "/csi_cumper")) {
  csi.breaks <- c(-1000, -2, -1.6, -1.3, -0.8, -0.5, 0.5, 0.8, 1.3, 1.6, 2, 1000)
  csi.cols <- c("#730000CC", "#E60000CC", "#FFAA00CC", "#FCD37FCC", "#FEFF00CC", "#FDFFE4CC", "#E4E1EACC", "#B6D0E5CC", "#91ABD0CC", "#33648DCC", "#23425FCC")
  num_sites <- dim(csi)[3]
  num_months <- dim(csi)[1]
  scale <- dim(csi)[2]
  for (i in 1:num_sites)
    for (j in 1:scale) {
      t <- csi[, j, i]
      t <- t[!is.na(t)]
      t <- sort(t)
      s <- t - min(t)
      s <- 100 * cumsum(s) / sum(s)
      png(filename = paste0(dir, "/", dimnames(csi)[[3]][i], "_cumper.png"), width = 1724, height = 614, units = "px", pointsize = 12)
      par(mar = c(5.1, 4.1, 4.1, 4.1))
      plot(s, type = "n", ylim = c(0, max), ylab = "Coastal salinity index, interval in months", xlab = "Date", main = paste0(dimnames(csi)[[3]][j], " Coastal Salinity Index With 1-to-", scale, " Month Interval"), axes = F, frame.plot = T)
      for (i in 1:scale) {
        bin <- cut(unlist(csi[, i, j]), csi.breaks, labels = F)
        for (k in 1:num_months) if (!is.na(bin[k])) rect(as.numeric(xrange[k]), i * int_ht - int_ht, as.numeric(xrange[k + 1]), i * int_ht, col = csi.cols[bin[k]], border = NA)
      }
      abline(h = mean(sal[, j + 2], na.rm = T), lwd = 3, col = "grey28")
      abline(h = quantile(sal[, j + 2], c(.25, .75), na.rm = T), lwd = 3,col = "darkgrey")
      lines(xrange2, mwa[, j], lwd = 3, col = "darkblue")
      axis(1, as.numeric(seq.Date(as.Date(paste0(sal$Year[1], "/1/1")), as.Date(paste0(sal$Year[num_months], "/1/1")), by = "year")), sal$Year[1]:sal$Year[num_months], tck = 0.02)
      axis(2, seq(int_ht / 2, max, int_ht), 1:scale, tck = 0.02, las = 1)
      rect(as.numeric(xrange[num_months] + 150), -50, as.numeric(xrange[num_months]) +300, 5.25, col = "cyan1", border = NA)
      rect(as.numeric(xrange[num_months] + 150), 5.25, as.numeric(xrange[num_months]) +300, 18, col = "cyan2", border = NA)
      rect(as.numeric(xrange[num_months] + 150), 18, as.numeric(xrange[num_months]) +300, 30, col = "cyan3", border = NA)
      rect(as.numeric(xrange[num_months] + 150), 30, as.numeric(xrange[num_months]) +300, 40, col = "cyan4", border = NA)
      rect(as.numeric(xrange[num_months] + 150), 40, as.numeric(xrange[num_months]) +300, 80, col = "deepskyblue4", border = NA)
      axis(4, c(olig_p, 6, 19, 31, 41), c(olig, meso, poly, eu, hyper), tick = F, padj = -4.5, hadj = 0, font = 2)
      axis(4, c(5.25, 18, 30, 40), c(5, 18, 30, 40), lwd.ticks = 0.5, tck = 0.06, las = 1)
      mtext("Period of record values and estuarine salinity ranges, in practical salinity units", 4, 3)
      fst <- which(!is.na(sal[, j + 2]))[1]
      lst <- tail(which(!is.na(sal[, j + 2])), 1)
      tmp <- legend("topleft", c("", "", "", "12-month rolling salinity average", "Mean", "25th and 75th percentile", "", "", "", "", "", ""), lty = c(NA, NA, NA, 1, 1, 1, NA, NA, NA, NA, NA, NA), lwd = c(NA, NA, NA, 3, 3, 3, NA, NA, NA, NA, NA, NA), col = c(NA, NA, NA, "darkblue", "grey28", "darkgrey", NA, NA, NA, NA, NA, NA), inset = c(0.01, 0.01), title = expression(bold("EXPLANATION")))
      rect(c(9330, 9330, 9330, 9330, 9330, 9530, 9830, 9830, 9830, 9830, 9830), c(28.5, 27, 25.5, 24, 22.5, 22.5, 28.5, 27, 25.5, 24, 22.5), c(9490, 9490, 9490, 9490, 9490, 9790, 9990, 9990, 9990, 9990, 9990), c(29.8, 28.3, 26.8, 25.3, 23.8, 23.8, 29.8, 28.3, 26.8, 25.3, 23.8), lwd = 2, col = c(csi.cols[1:6], rev(csi.cols[7:11])))
      text(c(9410, 9410, 9410, 9410, 9410, 9660, 9910, 9910, 9910, 9910, 9910), c(29.15, 27.65, 26.15, 24.65, 23.15, 23.15, 29.15, 27.65, 26.15, 24.65, 23.15), c("CD4", "CD3", "CD2", "CD1", "CD0", "Normal", "CW4", "CW3", "CW2", "CW1", "CW0"))
      text(tmp$rect$left + tmp$rect$w, tmp$text$y[1:2], c("CD, coastal drought; CW, coastal wet", paste0("Period of record: ", sal$Month[fst], "/", sal$Year[fst], " - ", sal$Month[lst], "/", sal$Year[lst])), pos = 2)
      dev.off()
    }
}
