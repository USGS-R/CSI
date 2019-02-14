#' @title Create individual CSI interval plots for sites
#'
#' @description For each site (layer) in the csi object, plot each interval (column).
#'
#' @param csi array A 3D array of CSI values with dimensions of number of months covered, scale of months analysed (typically 1-24), and number of sites.
#' @param dir character Directory to write output files to.
#' @param leg character If "topleft" (default), legend will be displayed in upper left corner; if "bottom", legend will be placed horizontally along the figure bottom. Else no legend will be displayed.
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
CSIplot <- function (csi, dir = paste0(getwd(), "/csi_plots"), leg = "topleft") {
  if (!(length(dir) == 1) || !is.character(dir))
    stop("dir must be a single character string")
  if (!dir.exists(dir)) dir.create(dir)
  csi.breaks <- c(-1000, -2, -1.6, -1.3, -0.8, -0.5, 0.5, 0.8, 1.3, 1.6, 2, 1000)
  csi.cols <- c("#740200CC", "#E80600CC", "#FFAC00CC", "#FCD57BCC", "#FEFF00CC", "#FDFFE4CC", "#E4E1EACC", "#B6D0E5CC", "#91ABD0CC", "#33648DCC", "#23425FCC")
  xrange <- seq.Date(as.Date(paste(dimnames(csi)[[1]][1], "01", sep = "-")), as.Date(paste(rev(dimnames(csi)[[1]])[1], "01" , sep = "-")), by = "month")
  xrange2 <- xrange + 15 # Offset to plot midmonth
  sal <- attr(csi, "sal")
  filled_gaps <- attr(csi, "filled_gaps")
  max <- ceiling(max(sal[, 3:dim(sal)[2]], na.rm = T))
  num_sites <- dim(csi)[3]
  num_months <- dim(csi)[1]
  scale <- dim(csi)[2]

  for (j in 1:num_sites) {
    gaps <- filled_gaps[[paste0(dimnames(csi)[[3]][j], "_filled_gaps")]]
    gaplengths <- filled_gaps[[paste0(dimnames(csi)[[3]][j], "_filled_gaps_len")]]
    for (i in 1:scale) {
      bin <- cut(unlist(csi[, i, j]), csi.breaks, labels = F)
      st <- which(!is.na(sal[, j + 2]))[1]
      en <- rev(which(!is.na(sal[, j + 2])))[1]
      mwa <- NULL # Moving-window average of same length as scale
      for (k in st:(en - i + 1))
        mwa[k] <- mean(sal[k:(k + i - 1), j + 2], na.rm = T)
      mwa <- c(rep(NA, num_months - length(mwa)), mwa) # pad
      if (leg == "bottom") { m <- c(8.1, 4.1, 4.1, 4.1); ht <- 659 } else { m <- c(5.1, 4.1, 4.1, 4.1); ht <- 614 }
      png(paste0(dir, "/", dimnames(csi)[[3]][j], "_interval", i, ".png"), width = 1724, height = ht)
      par(mar = m)
      plot(xrange, sal[, j + 2], type = "n", ylim = c(0, max), ylab = paste0(i, "-month average salinity (PPT)"), xlab = "Date", main = paste0(dimnames(csi)[[3]][j], " ", i, "-month CSI (background) and ", i, "-month average salinity (black trace)"), xaxt = "n", tck = 0.02, cex.axis = 1.25, cex.lab = 1.25, las = 1)
      for (k in 1:num_months) rect(as.numeric(xrange[k]), 0, as.numeric(xrange[k + 1]), 56, col = csi.cols[bin[k]], border = NA)
      for (k in which(!is.na(bin))[1]:rev(which(!is.na(bin)))[1]) if (is.na(bin[k])) rect(as.numeric(xrange[k]), 0, as.numeric(xrange[k + 1]), 56, col = "gray25", border = NA)
      if (i == 1 & !is.null(gaps))
        for (k in 1:length(gaps)) {
          st <- as.Date(paste0(gaps[k], "-01"))
          en <- seq(st, by = paste(gaplengths[k], "month"), length = 2)[2]
          rect(st, 0, en, max * 1.035, border = "limegreen", lwd = 3)
        }
      axis(1, as.numeric(seq.Date(as.Date(paste0(sal$Year[1], "/1/1")), as.Date(paste0(sal$Year[num_months], "/1/1")), by = "year")), sal$Year[1]:sal$Year[num_months], tck = 0.02, cex.axis = 1.25)
      #    par(new=T) # Optional 1-month interval line. To remove, comment out from this line...
      #    plot(xrange,csi[,1,j],type="l",ylim=range(csi,na.rm=T),xaxt="n",yaxt="n",xlab="",ylab="",lwd=3,col="dodgerblue")
      #    axis(4)
      #    mtext("One month interval CSI",side=4,line=3) # ...to this line.
      par(new = T)
      plot(xrange2, mwa, lwd = 3, type = "l", ylim = range(sal[, j + 2], na.rm = T), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
      fst <- which(!is.na(sal[, j + 2]))[1]
      lst <- tail(which(!is.na(sal[, j + 2])), 1)
      leg_txt <- c("", "", "", "", "", "", "", "", "", paste0(i, "-month rolling salinity average"))
      leg_lty <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 1)
      leg_lwd <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 3)
      leg_col <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "black")
      leg_col2 <- c(csi.cols[1:6], "gray25", rev(csi.cols[7:11]))
      leg_exp <- c("CD, coastal drought; CW, coastal wet", paste0("Period of record: ", sal$Month[fst], "/", sal$Year[fst], " - ", sal$Month[lst], "/", sal$Year[lst]))
      leg_txt2 <- c("CD4", "CD3", "CD2", "CD1", "CD0", "Normal", "Missing", "CW4", "CW3", "CW2", "CW1", "CW0")
      seg_b <- 0.59
      seg_e <- 0.60
      if (i == 1 & !is.null(gaps)) { leg_txt[11] <- "Interpolated data"; leg_lty[11] <- 1; leg_lwd[11] <- 3; leg_col[11] <- "red"; seg_b <- c(seg_b, 0.717); seg_e <- c(seg_e, 0.727) }
      if (leg == "bottom") {
        par(xpd = T)
        par(usr = c(0, 1, 0, 1))
        text(0.58, -0.1, "EXPLANATION: ", font = 2, pos = 4)
        text(0.64, -0.1, paste0("CD, coastal drought; CW, coastal wet; Period of record: ", sal$Month[fst], "/", sal$Year[fst], " - ", sal$Month[lst], "/", sal$Year[lst]), pos = 4)
        text(0.6, -0.186, paste(leg_txt[-(1:9)], collapse = "           "), pos = 4)
        segments(seg_b, -0.186, seg_e, -0.186, lty = leg_lty[-(1:9)], lwd = 3, col = leg_col[-(1:9)])
        rect(c(0.59, 0.62, 0.65, 0.68, 0.71, 0.74, 0.93, 0.77, 0.8, 0.83, 0.86, 0.89), c(-0.124, -0.124, -0.124, -0.124, -0.124, -0.124, -0.124, -0.124, -0.124, -0.124, -0.124, -0.124), c(0.62, 0.65, 0.68, 0.71, 0.74, 0.77, 0.96, 0.80, 0.83, 0.86, 0.89, 0.92), c(-0.156, -0.156, -0.156, -0.156, -0.156, -0.156, -0.156, -0.156, -0.156, -0.156, -0.156, -0.156), lwd = 2, col = leg_col2[c(1:7, 12:8)])
        text(c(0.605, 0.635, 0.665, 0.695, 0.725, 0.755, 0.945, 0.785, 0.815, 0.845, 0.875, 0.905), c(-0.14, -0.14, -0.14, -0.14, -0.14, -0.14, -0.14, -0.14, -0.14, -0.14, -0.14, -0.14), leg_txt2[c(1:7, 12:8)], col = c(rep("black", 6), "white", rep("black", 5)))
      }
      if (leg == "topleft") {
        tmp <- legend("topleft", leg_txt, lty = leg_lty, lwd = leg_lwd, col = leg_col, inset = c(0.01, 0.01), title = expression(bold("EXPLANATION")))
        text(tmp$rect$left + tmp$rect$w, tmp$text$y[1:2], leg_exp, pos = 2)
        par(usr = c(0, 1, 0, 1))
        rect(c(0.028, 0.028, 0.028, 0.028, 0.028, 0.053, 0.053, 0.091, 0.091, 0.091, 0.091, 0.091), c(0.834, 0.796, 0.758, 0.720, 0.682, 0.682, 0.834, 0.834, 0.796, 0.758, 0.720, 0.682), c(0.048, 0.048, 0.048, 0.048, 0.048, 0.086, 0.086, 0.111, 0.111, 0.111, 0.111, 0.111), c(0.866, 0.828, 0.790, 0.752, 0.714, 0.714, 0.866, 0.866, 0.828, 0.790, 0.752, 0.714), lwd = 2, col = leg_col2)
        text(c(0.038, 0.038, 0.038, 0.038, 0.038, 0.070, 0.070, 0.101, 0.101, 0.101, 0.101, 0.101), c(0.850, 0.812, 0.774, 0.736, 0.698, 0.698, 0.850, 0.850, 0.812, 0.774, 0.736, 0.698), leg_txt2, col = c(rep("black", 6), "white", rep("black", 5)))
      }
      dev.off()
    }
  }
}
