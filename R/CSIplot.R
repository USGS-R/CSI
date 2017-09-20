#' @title Create individual CSI interval plots for sites
#'
#' @description For each site (layer) in the csi object, plot each interval (column).
#'
#' @param csi array A 3D array of CSI values with dimensions of number of months covered, scale of months analysed (typically 1-24), and number of sites.
#' @param dir character Directory to write output files to.
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
CSIplot <- function (csi, dir = paste0(getwd(), "/csi_plots")) {
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

  for (j in 1:num_sites)
    for (i in 1:scale) {
      bin <- cut(unlist(csi[, i, j]), csi.breaks, labels = F)
      mwa <- NULL # Moving-window average of same length as scale
      for (k in 1:(num_months - i + 1))
        mwa[k] <- mean(sal[k:(k + i - 1), j + 2])
      mwa <- c(rep(NA, num_months - length(mwa)), mwa) # pad
      png(filename = paste0(dir, "/", dimnames(csi)[[3]][j], "_interval", i, ".png"), width = 1724, height = 614, units = "px", pointsize = 12, type = "quartz")
      par(mar = c(5, 4, 4, 5) + .1)
      plot(yrange, sal[, j + 2], type = "n", ylim = c(0, max), ylab = paste0(i, "-month average salinity (PPT)"), xlab = "Date", main = paste0(dimnames(csi)[[3]][j], " ", i, "-month CSI (background) and ", i, "-month average salinity (black trace)"), xaxt = "n")
      for (k in 1:num_months) if (!is.na(bin[k])) rect(as.numeric(yrange[k]), 0, as.numeric(yrange[k + 1]), 56, col = csi.cols[bin[k]], border = NA)
      axis(1, as.numeric(seq.Date(as.Date(paste0(sal$Year[1], "/1/1")), as.Date(paste0(sal$Year[num_months], "/1/1")), by = "year")), sal$Year[1]:sal$Year[num_months])
      #    par(new=T) # Optional 1-month interval line. To remove, comment out from this line...
      #    plot(yrange,csi[,1,j],type="l",ylim=range(csi,na.rm=T),xaxt="n",yaxt="n",xlab="",ylab="",lwd=3,col="dodgerblue")
      #    axis(4)
      #    mtext("One month interval CSI",side=4,line=3) # ...to this line.
      par(new = T)
      plot(yrange2, mwa, lwd = 3, type = "l", ylim = range(sal[, j + 2], na.rm = T), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
      dev.off()
    }
}
