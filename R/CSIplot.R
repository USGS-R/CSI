#' @title Create individual CSI interval plots for sites
#'
#' @description For each site (layer) in the csi object, plot each interval (column).
#'
#' @param csi array A 3D array of CSI values with dimensions of number of months covered, scale of months analysed (typically 1-24), and number of sites.
#' @param dir character Directory to write output files to.
#' @param leg logical. If true, legend will be displayed in upper left corner.
#'
#' @importFrom grDevices dev.off png
#' @importFrom graphics axis par plot rect abline legend lines mtext
#' @importFrom stats quantile
#' @importFrom utils tail
#'
#' @export
#'
#' @examples
#' # Data file with Year and Month columns
#' data_path <- system.file("extdata", "Monthly_Coastal_EDEN.csv", package = "CSI")
#' sal <- CSIimport_monthly(data_path)
#' csi <- CSIcalc(sal)
#' CSIplot(csi)
#'
CSIplot <- function (csi, dir = paste0(getwd(), "/csi_plots"), leg = T) {
  if (!(length(dir) == 1) || !is.character(dir))
    stop("dir must be a single character string")
  if (!dir.exists(dir)) dir.create(dir)
  csi.breaks <- c(-1000, -2, -1.6, -1.3, -0.8, -0.5, 0.5, 0.8, 1.3, 1.6, 2, 1000)
  csi.cols <- c("#740200CC", "#E80600CC", "#FFAC00CC", "#FCD57BCC", "#FEFF00CC", "#FDFFE4CC", "#E4E1EACC", "#B6D0E5CC", "#91ABD0CC", "#33648DCC", "#23425FCC")
  xrange <- seq.Date(as.Date(paste(dimnames(csi)[[1]][1], "01", sep = "-")), as.Date(paste(rev(dimnames(csi)[[1]])[1], "01" , sep = "-")), by = "month")
  xrange2 <- xrange + 15 # Offset to plot midmonth
  sal <- attr(csi, "sal")
  max <- ceiling(max(sal[, 3:dim(sal)[2]], na.rm = T))
  num_sites <- dim(csi)[3]
  num_months <- dim(csi)[1]
  scale <- dim(csi)[2]

  for (j in 1:num_sites)
    for (i in 1:scale) {
      bin <- cut(unlist(csi[, i, j]), csi.breaks, labels = F)
      st <- which(!is.na(sal[, j + 2]))[1]
      en <- rev(which(!is.na(sal[, j + 2])))[1]
      mwa <- NULL # Moving-window average of same length as scale
      for (k in st:(en - i + 1))
        mwa[k] <- mean(sal[k:(k + i - 1), j + 2], na.rm = T)
      mwa <- c(rep(NA, num_months - length(mwa)), mwa) # pad
      png(filename = paste0(dir, "/", dimnames(csi)[[3]][j], "_interval", i, ".png"), width = 1724, height = 614, units = "px", pointsize = 12)
      par(mar = c(5, 4, 4, 5) + .1)
      plot(xrange, sal[, j + 2], type = "n", ylim = c(0, max), ylab = paste0(i, "-month average salinity (PPT)"), xlab = "Date", main = paste0(dimnames(csi)[[3]][j], " ", i, "-month CSI (background) and ", i, "-month average salinity (black trace)"), xaxt = "n", tck = 0.02, cex.axis = 1.25, cex.lab = 1.25, las = 1)
      for (k in 1:num_months) rect(as.numeric(xrange[k]), 0, as.numeric(xrange[k + 1]), 56, col = csi.cols[bin[k]], border = NA)
      for (k in which(!is.na(bin))[1]:rev(which(!is.na(bin)))[1]) if (is.na(bin[k])) rect(as.numeric(xrange[k]), 0, as.numeric(xrange[k + 1]), 56, col = "gray25", border = NA)
      axis(1, as.numeric(seq.Date(as.Date(paste0(sal$Year[1], "/1/1")), as.Date(paste0(sal$Year[num_months], "/1/1")), by = "year")), sal$Year[1]:sal$Year[num_months], tck = 0.02, cex.axis = 1.25)
      #    par(new=T) # Optional 1-month interval line. To remove, comment out from this line...
      #    plot(xrange,csi[,1,j],type="l",ylim=range(csi,na.rm=T),xaxt="n",yaxt="n",xlab="",ylab="",lwd=3,col="dodgerblue")
      #    axis(4)
      #    mtext("One month interval CSI",side=4,line=3) # ...to this line.
      par(new = T)
      plot(xrange2, mwa, lwd = 3, type = "l", ylim = range(sal[, j + 2], na.rm = T), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
      if (leg) {
        fst <- which(!is.na(sal[, j + 2]))[1]
        lst <- tail(which(!is.na(sal[, j + 2])), 1)
        tmp <- legend("topleft", c("", "", "", "", "", "", "", "", "", paste0(i, "-month rolling salinity average")), lty = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 1), lwd = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 3), col = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "black"), inset = c(0.01, 0.01), title = expression(bold("EXPLANATION")))
        text(tmp$rect$left + tmp$rect$w, tmp$text$y[1:2], c("CD, coastal drought; CW, coastal wet", paste0("Period of record: ", sal$Month[fst], "/", sal$Year[fst], " - ", sal$Month[lst], "/", sal$Year[lst])), pos = 2)
        par(usr = c(0, 1, 0, 1))
        rect(c(0.028, 0.028, 0.028, 0.028, 0.028, 0.053, 0.091, 0.091, 0.091, 0.091, 0.091), c(0.834, 0.796, 0.758, 0.720, 0.682, 0.682, 0.834, 0.796, 0.758, 0.720, 0.682), c(0.048, 0.048, 0.048, 0.048, 0.048, 0.086, 0.111, 0.111, 0.111, 0.111, 0.111), c(0.866, 0.828, 0.790, 0.752, 0.714, 0.714, 0.866, 0.828, 0.790, 0.752, 0.714), lwd = 2, col = c(csi.cols[1:6], rev(csi.cols[7:11])))
        text(c(0.038, 0.038, 0.038, 0.038, 0.038, 0.070, 0.101, 0.101, 0.101, 0.101, 0.101), c(0.850, 0.812, 0.774, 0.736, 0.698, 0.698, 0.850, 0.812, 0.774, 0.736, 0.698), c("CD4", "CD3", "CD2", "CD1", "CD0", "Normal", "CW4", "CW3", "CW2", "CW1", "CW0"))
      }
      dev.off()
    }
}
