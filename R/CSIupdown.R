#' @title Create individual CSI interval up/down plots for sites
#'
#' @description For each site (layer) in the csi object, plot each interval (column).
#'
#' @param csi array A 3D array of CSI values with dimensions of number of months covered, scale of months analysed (typically 1-24), and number of sites.
#' @param dir character Directory to write output files to.
#'
#' @importFrom grDevices dev.off png
#' @importFrom graphics axis par plot abline lines polygon
#'
#' @export
#'
#' @examples
#' # Data file with Year and Month columns
#' data_path <- system.file("extdata", "Monthly_Coastal_EDEN.csv", package = "CSI")
#' sal <- CSIimport_monthly(data_path)
#' csi <- CSIcalc(sal)
#' CSIupdown(csi)
#'
CSIupdown <- function (csi, dir = paste0(getwd(), "/csi_updowns")) {
  if (!(length(dir) == 1) || !is.character(dir))
    stop("dir must be a single character string")
  if (!dir.exists(dir)) dir.create(dir)
  csi.breaks <- c(-1000, -2, -1.6, -1.3, -0.8, -0.5, 0.5, 0.8, 1.3, 1.6, 2, 1000)
  xrange <- seq.Date(as.Date(paste(dimnames(csi)[[1]][1], "01", sep = "-")), as.Date(paste(rev(dimnames(csi)[[1]])[1], "01" , sep = "-")), by = "month")
  rnge <- max(abs(csi), na.rm = T)
  sal <- attr(csi, "sal")
  num_sites <- dim(csi)[3]
  num_months <- dim(csi)[1]
  scale <- dim(csi)[2]

  for (j in 1:num_sites)
    for (i in 1:scale) {
      png(paste0(dir, "/", dimnames(csi)[[3]][j], "_updown", i, ".png"), width = 1724, height = 614)
      par(mar = c(5, 4, 4, 5) + .1)
      plot(xrange, csi[, i, j], type = "n", ylim = c(-rnge, rnge), ylab = paste0(i, "-month CSI"), xlab = "Date", main = paste0(dimnames(csi)[[3]][j], " ", i, "-Month Coastal Salinity Index"), xaxt = "n", cex.lab = 1.25, cex.axis = 1.25, tck = 0.02, las = 1)
      axis(1, as.numeric(seq.Date(as.Date(paste0(sal$Year[1], "/1/1")), as.Date(paste0(sal$Year[num_months], "/1/1")), by = "year")), sal$Year[1]:sal$Year[num_months], cex.axis = 1.25, tck = 0.02)
      abline(v = seq.Date(as.Date(paste0(sal$Year[1], "-01-01")), as.Date(paste0(rev(sal$Year)[1], "-01-01")), by = "year"))
      abline(h = 0)
      posx <- negx <- xrange[which(!is.na(csi[, i, j]))[1]:rev(which(!is.na(csi[, i, j])))[1]]
      posy <- negy <- csi[, i, j][which(!is.na(csi[, i, j]))[1]:rev(which(!is.na(csi[, i, j])))[1]]
      posy[posy < 0 | is.na(posy)] <- 0
      if (posy[1] != 0) { posy <- c(0, posy); posx <- c(posx[1], posx) }
      if (posy[length(posy)] != 0) { posy <- c(posy, 0); posx <- c(posx, posx[length(posx)]) }
      negy[negy > 0 | is.na(negy)] <- 0
      if (negy[1] != 0) { negy <- c(0, negy); negx <- c(negx[1], negx) }
      if (negy[length(negy)] != 0) { negy <- c(negy, 0); negx <- c(negx, negx[length(negx)]) }
      polygon(posx, posy, col = "blue")
      polygon(negx, negy, col = "red")
      lines(xrange, csi[, i, j], lwd = 3, type = "l")
      dev.off()
    }
}
