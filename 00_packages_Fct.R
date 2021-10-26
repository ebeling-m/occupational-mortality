# Packages and functiosn for estimation
library(tidyverse)
library(magrittr)
library(mgcv)
library(xtable)

# Additional Function borrowed from plyr package

mapvalues <- function (x, from, to, warn_missing = TRUE) 
{
  if (length(from) != length(to)) {
    stop("`from` and `to` vectors are not the same length.")
  }
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector.")
  }
  if (is.factor(x)) {
    levels(x) <- mapvalues(levels(x), from, to, warn_missing)
    return(x)
  }
  mapidx <- match(x, from)
  mapidxNA <- is.na(mapidx)
  from_found <- sort(unique(mapidx))
  if (warn_missing && length(from_found) != length(from)) {
    message("The following `from` values were not present in `x`: ", 
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
  }
  x[!mapidxNA] <- to[mapidx[!mapidxNA]]
  x
}

# Function for partial life expectancy calculation

partialLe <- function(mx, occ, sex, age, startAge){
  ax <- rep(0.5, length(mx))
  qx <- mx/(1+(1-ax)*mx)
  px <- 1-qx
  lx <- c(1000, (cumprod(px)*1000))
  dx <- -diff(lx)
  Lx <- lx[-1]+ax*dx
  Tx <- rev(cumsum(rev(Lx)))
  pE <- Tx/lx[-length(lx)]
  out <- data.frame(age = age, occ=occ, sex=sex, startAge = startAge,pE = pE)
  return(out)  
}

# Same function maxAge for fixed min/max calculation

Lx <- function(mx, occ, sex, age, startAge){
  ax <- rep(0.5, length(mx))
  qx <- mx/(1+(1-ax)*mx)
  px <- 1-qx
  lx <- c(1000, (cumprod(px)*1000))
  dx <- -diff(lx)
  Lx <- lx[-1]+ax*dx
  Tx <- rev(cumsum(rev(Lx)))
  pE <- Lx/lx[-length(lx)]
  out <- data.frame(age = age, occ=occ, sex=sex, startAge = startAge, Lx = Lx, lx = lx[-length(lx)])
  return(out)  
}
