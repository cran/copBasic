sg2brix <- function(sg, tempF=NULL) {
  corr <- 0
  if(! is.null(tempF)) {
     temperF <- c(54.2, 61.5, 68, 73.7, 79.2, 84.3)
     sg_corr <- c(-0.002, -0.001, 0, +0.001, +0.002, +0.003)
     corr <- stats::approx(temperF, sg_corr, xout=tempF[1], rule=2)$y
  }
  sg <- sg+corr
  bx <- 182.4601*sg^3 - 775.6821*sg^2 + 1262.7794*sg - 669.5622
  # Source: http://en.wikipedia.org/wiki/Brix
  return(bx)
}

brix2sg <- function(bx, tempF=NULL) {

  # sg <- (bx / (258.6-((bx / 258.2)*227.1))) + 1
  # print(sg) https://www.brewersfriend.com/brix-converter/

  afunc <- function(sg, bx) sg2brix(sg) - bx
  rt <- NULL
  try(rt <- uniroot(afunc, interval=c(.5,2), bx=bx))
  if(is.null(rt)) return(NA)
  sg <- rt$root
  corr <- 0
  if(! is.null(tempF)) {
     temperF <- c(54.2, 61.5, 68, 73.7, 79.2, 84.3)
     sg_corr <- c(-0.002, -0.001, 0, +0.001, +0.002, +0.003)
     corr <- stats::approx(temperF, sg_corr, xout=tempF[1], rule=2)$y
  }
  return(round(sg+corr, digits=4))
}



refractometer_wferm <-
function(original_brix_hydrometer, apparent_brix_refractometer_wferm, tempF=NULL) {
  ob <- original_brix_hydrometer[1] #
  ab <- apparent_brix_refractometer_wferm[1]

  sg2brix_morewine <- function(sg) {
    sg <- sg[1]
    return(190.74*sg^3 - 800.47*sg^2 + 1286.4*sg - 676.67) # MoreWine.com
  }


  # MoreWine.com
  sg <- 1.001843 - 0.002318474 * ob - 7.775E-6 * ob^2 - 3.4E-8 * ob^3 +
                   0.005740000 * ab + 3.344E-5 * ab^2 + 8.6E-8 * ab^3

  corr <- 0
  if(! is.null(tempF)) {
     temperF <- c(54.2, 61.5, 68, 73.7, 79.2, 84.3)
     sg_corr <- c(-0.002, -0.001, 0, +0.001, +0.002, +0.003)
     corr <- stats::approx(temperF, sg_corr, xout=tempF[1], rule=2)$y
  }
  sg <- sg+corr
  return(list(actual_specific_gravity=round(sg, digits=4),
              actual_brix=round(sg2brix_morewine(sg), digits=4)))
}

refractometer_wferm(24.5, 15) # 1.034, 8.5
