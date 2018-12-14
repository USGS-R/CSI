#' @title Create stacked CSI interval plots for sites
#'
#' @description For each site (layer) in the csi object, plot the intervals (columns) on a stacked plot.
#'
#' @param csi array A 3D array of CSI values with dimensions of number of months covered, scale of months analysed (typically 1-24), and number of sites.
#' @param dir character Directory to write output files to.
#' @param thumbs logical. If true, thumbnail plots will be generated in addition to full-sized plots.
#' @param grouped logical. If true, second y-asix (salinity) will be the same across all output plots.
#' @param leg logical. If true, legend will be displayed in upper left corner.
#'
#' @importFrom grDevices dev.off png
#' @importFrom graphics axis par plot rect text
#' @importFrom utils tail
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
CSIstack <- function (csi, dir = paste0(getwd(), "/csi_stacked"), thumbs = F, grouped = T, leg = T) {
  if (!(length(dir) == 1) || !is.character(dir))
    stop("dir must be a single character string")
  if (!dir.exists(dir)) dir.create(dir)
  csi.breaks <- c(-1000, -2, -1.6, -1.3, -0.8, -0.5, 0.5, 0.8, 1.3, 1.6, 2, 1000)
  csi.cols <- c("#730000CC", "#E60000CC", "#FFAA00CC", "#FCD37FCC", "#FEFF00CC", "#FDFFE4CC", "#E4E1EACC", "#B6D0E5CC", "#91ABD0CC", "#33648DCC", "#23425FCC")
  xrange <- seq.Date(as.Date(paste(dimnames(csi)[[1]][1], "01", sep = "-")), as.Date(paste(rev(dimnames(csi)[[1]])[1], "01" , sep = "-")), by = "month")
  xrange2 <- xrange + 15 # Offset to plot midmonth
  sal <- attr(csi, "sal")
  filled_gaps <- attr(csi, "filled_gaps")
  num_sites <- dim(csi)[3]
  num_months <- dim(csi)[1]
  mwa <- array(NA, c(num_months - 11, num_sites)) # 12-month moving-window average
  for (j in 1:num_sites) {
    st <- which(!is.na(sal[, j + 2]))[1]
    en <- rev(which(!is.na(sal[, j + 2])))[1]
    for (k in st:(en - 11))
      if (!is.na(sal[k + 11, j + 2]))
        mwa[k, j] <- mean(sal[k:(k + 11), j + 2], na.rm = T)
  }
  mwa <- rbind(array(NA, c(11, num_sites)), mwa)
  for (j in 1:num_sites) {
    max <- if (grouped) ceiling(max(mwa, na.rm = T)) * 1.1 else max(mwa[, j], na.rm = T) * 1.1
    scale <- dim(csi)[2]
    int_ht <- max / scale # Height of scale interval for stacked plot
    olig <- if (max > 41) "Olig." else "Oligohaline"
    olig_p <- if (max >= 25) -1 else 0
    meso <- if (max > 6.25) "Mesohaline" else NA
    poly <- if (max > 20.25) "Polyhaline" else NA
    eu <- if (max > 32) "Euhaline" else NA
    hyper <- if (max > 46) "Hypersaline" else if (max > 42.5) "Hyper" else NA
    gaps <- filled_gaps[[paste0(dimnames(csi)[[3]][j], "_filled_gaps")]]
    gaplengths <- filled_gaps[[paste0(dimnames(csi)[[3]][j], "_filled_gaps_len")]]
    if (thumbs) {
      png(paste0(dir, "/", dimnames(csi)[[3]][j], "_stacked_thumb.png"), width = 360, height = 150, pointsize = 2)
      par(mar = c(0, 0, 0, 0) + .1)
      plot(xrange, sal[, j + 2], type = "n", ylim = c(0, max), ylab = "", xlab = "", main = "", xaxt = "n", yaxt = "n", axes = F, frame.plot = T)
      for (i in 1:24) {
        bin <- cut(unlist(csi[, i, j]), csi.breaks, labels = F)
        for (k in 1:num_months) rect(as.numeric(xrange[k]), i * int_ht - int_ht, as.numeric(xrange[k + 1]),i * int_ht, col = csi.cols[bin[k]], border = NA)
        for (k in which(!is.na(bin))[1]:rev(which(!is.na(bin)))[1]) if (is.na(bin[k])) rect(as.numeric(xrange[k]), i * int_ht - int_ht, as.numeric(xrange[k + 1]), i * int_ht, col = "gray25", border = NA)
      }
      lines(xrange2, mwa[, j], lwd = 2)
      dev.off()
    }
    png(paste0(dir, "/", dimnames(csi)[[3]][j], "_stacked.png"), width = 1724, height = 614)
    par(mar = c(5.1, 4.1, 4.1, 4.1))
    plot(xrange, sal[, j + 2], type = "n", ylim = c(0, max), ylab = "Coastal salinity index interval, in months", xlab = "Date", main = paste0(dimnames(csi)[[3]][j], " Coastal Salinity Index With 1- to ", scale, "-Month Interval"), axes = F, frame.plot = T, cex.lab = 1.25)
    for (i in 1:scale) {
      bin <- cut(unlist(csi[, i, j]), csi.breaks, labels = F)
      for (k in 1:num_months) rect(as.numeric(xrange[k]), i * int_ht - int_ht, as.numeric(xrange[k + 1]), i * int_ht, col = csi.cols[bin[k]], border = NA)
      for (k in which(!is.na(bin))[1]:rev(which(!is.na(bin)))[1]) if (is.na(bin[k])) rect(as.numeric(xrange[k]), i * int_ht - int_ht, as.numeric(xrange[k + 1]), i * int_ht, col = "gray25", border = NA)
    }
    rm <- mwa_r <- mwa_rx <- NULL
    if (!is.null(gaps)) {
      for (i in 1:length(gaps)) {
        st <- as.Date(paste0(gaps[i], "-01"))
        en <- seq(st, by = paste(gaplengths[i], "month"), length = 2)[2]
        rect(as.numeric(st), 1 * int_ht - int_ht, as.numeric(en), 1 * int_ht, border = "limegreen", lwd = 3)
        tmp <- which(xrange == st):(which(xrange == en) - 1)
        rm <- c(rm, tmp)
        mwa_r <- c(mwa_r, mwa[(tmp[1] - 1):(rev(tmp)[1] + 1), j], NA)
        mwa_rx <- c(mwa_rx, (tmp[1] - 1):(rev(tmp)[1] + 2))
      }
      mwa[rm, j] <- NA
    }
    abline(h = mean(sal[, j + 2], na.rm = T), lwd = 3, col = "grey28")
    abline(h = quantile(sal[, j + 2], c(.25, .75), na.rm = T), lwd = 3,col = "darkgrey")
    lines(xrange2, mwa[, j], lwd = 3, col = "darkblue")
    if (!is.null(gaps)) lines(xrange2[mwa_rx], mwa_r, lwd = 3, lty = 3, col = "darkblue")
    axis(1, as.numeric(seq.Date(as.Date(paste0(sal$Year[1], "/1/1")), as.Date(paste0(sal$Year[num_months], "/1/1")), by = "year")), sal$Year[1]:sal$Year[num_months], tck = 0.02, cex.axis = 1.25)
    axis(2, seq(int_ht / 2, max, int_ht), 1:scale, tck = 0.02, las = 1, cex.axis = 1.25)
    rect(as.numeric(xrange[num_months] + 150), -50, as.numeric(xrange[num_months]) + 5000, 5, col = "cyan1", border = NA)
    rect(as.numeric(xrange[num_months] + 150), 5, as.numeric(xrange[num_months]) + 5000, 18, col = "cyan2", border = NA)
    rect(as.numeric(xrange[num_months] + 150), 18, as.numeric(xrange[num_months]) + 5000, 30, col = "cyan3", border = NA)
    rect(as.numeric(xrange[num_months] + 150), 30, as.numeric(xrange[num_months]) + 5000, 40, col = "cyan4", border = NA)
    rect(as.numeric(xrange[num_months] + 150), 40, as.numeric(xrange[num_months]) + 5000, 80, col = "deepskyblue4", border = NA)
    axis(4, c(olig_p, 6, 19, 31, 41), c(olig, meso, poly, eu, hyper), tick = F, padj = -4.5, hadj = 0, font = 2)
    axis(4, c(5, 18, 30, 40), c(5, 18, 30, 40), lwd.ticks = 0.5, tck = 0.06, las = 1, cex.axis = 1.25)
    axis(4, 0:50, F, lwd.ticks = 0.5, tck = 0.01)
    mtext("Period of record values and estuarine salinity ranges, in practical salinity units", 4, 2.75, cex = 1.15)
    if (leg) {
      fst <- which(!is.na(sal[, j + 2]))[1]
      lst <- tail(which(!is.na(sal[, j + 2])), 1)
      leg_txt <- c("", "", "", "", "", "", "", "", "", "Mean", "25th and 75th percentile", "12-month rolling salinity average")
      leg_lty <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 1, 1)
      leg_lwd <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 3, 3, 3)
      leg_col <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "grey28", "darkgrey", "darkblue")
      if (!is.null(gaps)) { leg_txt[13:14] <- c("Interpolated rolling average", "Interpolated data range"); leg_lty[13:14] <- c(3, 1); leg_lwd[13:14] <- 3; leg_col[13:14] <- c("darkblue", "limegreen") }
      tmp <- legend("topleft", leg_txt, lty = leg_lty, lwd = leg_lwd, col = leg_col, inset = c(0.01, 0.01), title = expression(bold("EXPLANATION")))
      text(tmp$rect$left + tmp$rect$w, tmp$text$y[1:2], c("CD, coastal drought; CW, coastal wet", paste0("Period of record: ", sal$Month[fst], "/", sal$Year[fst], " - ", sal$Month[lst], "/", sal$Year[lst])), pos = 2)
      par(usr = c(0, 1, 0, 1))
      rect(c(0.028, 0.028, 0.028, 0.028, 0.028, 0.053, 0.053, 0.091, 0.091, 0.091, 0.091, 0.091), c(0.834, 0.796, 0.758, 0.720, 0.682, 0.682, 0.834, 0.834, 0.796, 0.758, 0.720, 0.682), c(0.048, 0.048, 0.048, 0.048, 0.048, 0.086, 0.086, 0.111, 0.111, 0.111, 0.111, 0.111), c(0.866, 0.828, 0.790, 0.752, 0.714, 0.714, 0.866, 0.866, 0.828, 0.790, 0.752, 0.714), lwd = 2, col = c(csi.cols[1:6], "gray25", rev(csi.cols[7:11])))
      text(c(0.038, 0.038, 0.038, 0.038, 0.038, 0.070, 0.070, 0.101, 0.101, 0.101, 0.101, 0.101), c(0.850, 0.812, 0.774, 0.736, 0.698, 0.698, 0.850, 0.850, 0.812, 0.774, 0.736, 0.698), c("CD4", "CD3", "CD2", "CD1", "CD0", "Normal", "Missing", "CW4", "CW3", "CW2", "CW1", "CW0"), col = c(rep("black", 6), "white", rep("black", 5)))
    }
    dev.off()
  }
}
