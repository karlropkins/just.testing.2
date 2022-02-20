#####################
#' getFlightDistance
#####################
#'
#' getFlightDistance extracts aircraft flight distance from an online source, typically a API...
#' @param ref1,ref2 IATA code for department and arrival airports, respectively (character).
#' @param units units to report distance in, see below.
#' @param verbose if TRUE, function returns a report table else just answer in
#' requested units
#' @return If ref1 and ref2 are matched to known airports, the function outputs
#' reported distance
#' @keywords methods
#' @note This version of the function extracts results from the webflyer.com
#' mileage calculator: http://www.webflyer.com/travel/mileage_calculator
#' @export
#' @examples
#' #flight distance Los Angeles to London Heathrow
#' getFlightDistance("LAX", "LHR")

#KR & UE v.0.1 18/08/2016
#early example
#more recent code in Umoh's PhD

getFlightDistance <- function(ref1, ref2, units="km", verbose=TRUE){
  #am assuming ref1 and ref 2 are IATA Codes.
  #am using webflyer.com
  #am using XML readHTMLTable()
  api.url <- paste("http://www.webflyer.com/travel/mileage_calculator",
                   "/getmileage.php?city=", toupper(ref1), "&city=",
                   toupper(ref2), sep="")
  tab <- XML::readHTMLTable(api.url)
  #just want the results table
  tab <- tab[[length(tab)]]
  #check it does not think there is problem
  #and return their error message if there is...
  if(any(grepl("input error", tolower(names(tab))))){
    temp <- strsplit(as.character(tab[2,1]), "[.][.][.]")[[1]][1]
    temp <- gsub("our", "webflyer", temp)
    stop(temp)
  }
  #just want the one-way distances
  tab <- tab[1:(grep("one way distances", tolower(tab[,1]))[1]+1),]
  if(verbose) return(tab)
  #check we know requested units
  if(!units %in% c("miles", "km", "m"))
    stop("units unknown (miles, km or m)")
  if(units=="miles") {
    ans <- as.numeric(gsub("miles", "", tab[nrow(tab),2]))
  } else {
    ans <- as.numeric(gsub("km", "", tab[nrow(tab),3]))
    if(units=="m") ans <- ans*1000
  }
  ans
}
