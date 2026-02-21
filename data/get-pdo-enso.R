#' Extract PDO and ENSO Data
#'
#' @param years vector of year of interest
#' @param months vector of months of interest
#' @param lags vector of lags for months of interest
#'
#' @return list containing area and extent arrays(year,month), and description
#' @export
#'
#' @examples
get_pdo_npgo <- function(years=1963:2025, months=c(10,11,12,1,2,3,4,5), lags=c(-1,-1,-1,0,0,0,0,0)) {


  require(rsoi)
  
  # Tests
  if(length(months)!=length(lags)) { stop("Lengths of months and lags differ!") }
  
  # Metadata
  n.years <- length(years)
  n.months <- length(months)
  
  # Output object
  pdo <- array(dim=c(n.years, n.months), dimnames=list(years,paste("pdo",months,lags,sep="_")))
  enso <- array(dim=c(n.years, n.months), dimnames=list(years,paste("enso",months,lags,sep="_")))
  
  # Extract data
  pdo.dat <- download_pdo()  # Index derived as the leading PC of monthly SST anomalies in the North Pacific
  enso.dat <- download_mei()  # EOF of SLP, SST, zonal winds, surface winds, outgoing longwave radiation.
  
  # Fill in specific values
  y <- 1
  for(y in 1:n.years) {
    m <- 1
    for(m in 1:n.months) {
      
      # TESTING
      # (years[y]+lags[m])
      # 
      # months[m]
      # 
      # pdo.dat[pdo.dat$Year==(years[y]+lags[m]) &
      #           month(pdo.dat$Date)==months[m],]
      # npgo[y,m] <- npgo.dat$NPGO[npgo.dat$Year==(years[y]+lags[m]) &
                                   # npgo.dat$Month==month.abb[months[m]]]
      
      # PDO
      if(nrow(pdo.dat[pdo.dat$Year==(years[y]+lags[m]) &
                        month(pdo.dat$Date)==months[m],]) != 0) {
        pdo[y,m] <- pdo.dat$PDO[pdo.dat$Year==(years[y]+lags[m]) &
                                  month(pdo.dat$Date)==months[m]]
      }else {
        pdo[y,m] <- NA
      }
      
      
      # ENSO
      if(nrow(enso.dat[enso.dat$Year==(years[y]+lags[m]) &
                         month(enso.dat$Date)==months[m],]) != 0) {
        enso[y,m] <- enso.dat$MEI[enso.dat$Year==(years[y]+lags[m]) &
                                    month(enso.dat$Date)==months[m]]
      }else {
        enso[y,m] <- NA
      }

    } #next m
  } # next y
  
  # Return section
  out <- NULL
  out$pdo <- pdo
  out$enso <- enso
  out$description <- "PDO (Index derived as the leading PC of monthly SST anomalies in the North Pacific) and ENSO (EOF of SLP, SST, zonal winds, surface winds, outgoing longwave radiation.) summarized by month and year."
  return(out)
}

