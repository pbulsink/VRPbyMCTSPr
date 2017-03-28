# Prep Data and get Distance Matrix

#' Load Location Data
#'
#' Load location data from the file it is stored in.
#' Must be a .csv file with pattern: Name, StreetNumber, Street, City (Prov/State)
#' Example: Parliament Hill, 2, Wellington St., Ottawa Ontario
#'
#' @param f File of location data to load
#'
#' @return Locations as a data frame.
#' @export
#'
#' @examples locations <- loadLocationData('./museums.csv')
loadLocationData <- function(f) {
  locations <- read.csv(f, stringsAsFactors = FALSE)
  stopifnot(ncol(locations) == 4)
  stopifnot(names(locations) == c("Name", "Number", "Street", "City"))
  locations$Street <- gsub("\\.", "", locations$Street)
  locations$Number <- as.numeric(locations$Number)
  return(locations)
}

#' Build Request Lists
#'
#' Builds a list of request indices to send to Google Distance Matrix API under the free tier
#'
#' @param nlocations Total number of locations loaded from file. Functionally nrow(locations)
#'
#' @return A list of origins and destinations
#' @export
#'
#' @keywords internal
buildRequestList <- function(nlocations) {
  s_main <- 1:10
  s_last <- (1:(nlocations%%10)) + (10 * (nlocations%/%10))
  s_reps <- nlocations%/%10 - 1

  call_list <- list()
  counter <- 1
  for (i in 0:s_reps) {
    for (j in 0:s_reps) {
      r <- list(origin = s_main + i * 10, destin = s_main + j * 10)
      call_list[counter] <- list(r)
      counter <- counter + 1
    }
    r <- list(origin = s_last, destin = s_main + i * 10)
    call_list[counter] <- list(r)
    counter <- counter + 1
    r <- list(origin = s_main + i * 10, destin = s_last)
    call_list[counter] <- list(r)
    counter <- counter + 1
  }
  r <- list(origin = s_last, destin = s_last)
  call_list[counter] <- list(r)
  return(call_list)
}

#' Construct Distance URL
#'
#' Makes a Google Distance Matrix API compatable URL call
#'
#' @param origins The formatted origins for the API call
#' @param destinations The formatted destinations for the API call
#' @param dm_api_key The API key for Google Distance Matrix
#'
#' @return A URL prepared for Google Distance Matrix API
#' @export
#'
#' @keywords internal
#' @examples
constructDistanceUrl <- function(origins, destinations, dm_api_key) {
  root <- "https://maps.googleapis.com/maps/api/distancematrix/"
  u <- paste(root, "json", "?origins=", origins, "&destinations=", destinations,
    "&key=", dm_api_key, sep = "")
  return(u)
}

#' Get API URLS
#'
#' Automatically create API Call URLS from a list of locations.
#'
#' @param locations The locations as a data frame.
#' @param api_key_file The filename of the API key storage
#' @param ... Additional parametrs passed
#'
#' @return A list of URLS for Google Distance Matrix API calls
#' @export
#'
#' @examples getAPIurls(locations)
getAPIurls <- function(locations, api_key_file = "./data/apikey.txt", ...) {
  # Reformat location information for API Calls
  locations <- apply(locations, 1, function(x) paste(x[2], x[3], x[4]))
  locations <- trimws(locations)
  locations <- gsub(" ", "+", locations)

  # Get Request Indices
  request_indices <- buildRequestList(length(locations))

  # Get API key from FIlE
  apis <- read.table(api_key_file)
  dm_api_key <- as.character(apis[1, 1])

  urllist <- character()

  for (i in 1:length(request_indices)) {
    o <- request_indices[[i]]$origin
    origins <- paste0(locations[o], collapse = "|")
    d <- request_indices[[i]]$destin
    destinations <- paste0(locations[d], collapse = "|")
    urllist <- c(urllist, constructDistanceUrl(origins = origins, destinations = destinations,
      dm_api_key = dm_api_key))
  }

  return(urllist)
}

#' Process the Distance Matrix Return JSON
#'
#' @param jdata The raw returned json
#' @param value Whether to return a distance or duration (time) matrix
#'
#' @return
#' @export
#'
#' @keywords internal
#' @examples
processDistanceMatrixJson <- function(jdata, value = "distance", ...) {
  stopifnot(value %in% c("distance", "duration"))
  norigin <- length(jdata$origin_addresses)
  ndestin <- length(jdata$destination_addresses)
  dm <- matrix(NA, nrow = norigin, ncol = ndestin)
  for (i in 1:norigin) {
    for (j in 1:ndestin) {
      q <- jdata$rows[[i]]$elements[[j]]
      if (q$status == "OK") {
        dm[i, j] <- as.numeric(q[[value]]$value)
      }
    }
  }
  rownames(dm) <- jdata$origin_addresses
  colnames(dm) <- jdata$destination_addresses
  return(dm)
}

#' Get Google Distance Matrix Results
#'
#' For a list of urls get the distance and time matrices from the Google Distance Matrix API
#'
#' @param urllist the prebuilt API call urls
#'
#' @return a list of distance matrix, time matrix, raw reurned urls, jsons, and errors
#' @export
#'
#' @keywords internal
#' @examples
getGDMResults <- function(urllist) {
  dmat_list <- tmat_list <- rawurl_list <- json_list <- error_list <- list(rep(NA,
    length(urllist)))
  for (i in 1:length(urllist)) {
    rawurl_list[[i]] <- RCurl::getURL(urllist[i])
    json_list[[i]] <- rjson::fromJSON(rawurl_list[[i]])
    if (json_list[[i]]$status == "OK") {
      dmat_list[[i]] <- processDistanceMatrixJson(json_list[[i]], value = "distance")
      tmat_list[[i]] <- processDistanceMatrixJson(json_list[[i]], value = "duration")
    }
    Sys.sleep(2)
  }
  return(list(dmat = dmat_list, tmat = tmat_list, rawurl = rawurl_list, json = json_list,
    errors = error_list))
}

#' Assemble Matrix
#'
#' @param mlist The distance or time matrix list to compile into a single large matrix
#'
#' @return a single large matrix
#' @export
assembleMatrix <- function(mlist) {
  locations <- unique(as.character(unlist(sapply(mlist, rownames))))
  nloc <- length(locations)
  mat_total <- matrix(NA, nrow = nloc, ncol = nloc)
  rownames(mat_total) <- colnames(mat_total) <- locations
  for (i in 1:length(mlist)) {
    d <- mlist[[i]]
    if (!is.na(d) && !is.null(d)) {
        mat_total[match(rownames(d), rownames(mat_total),  nomatch=0), match(colnames(d), colnames(mat_total), nomatch = 0)]<-d
    }
  }
  return(mat_total)
}

#' Get Time And Distance Matrix
#'
#' Get the time and distance matrix for a list of supplied locations.
#'
#' @param locations A list of locations from loadLocationData
#' @param saveMatrixLists Boolean: whether to save to RDS the location data.
#' @param ... Additional parameters to pass
#'
#' @return a list of time and distance matrix
#' @export
#'
#' @examples getTimeAndDistanceMatrix(locations)
getTimeAndDistanceMatrix <- function(locations, saveMatrixLists = FALSE, ...) {
  url_list <- getAPIurls(locations, ...)
  results <- getGDMResults(urllist = url_list)
  if (saveMatrixLists) {
    saveRDS(results$dmat, "./dmat.RDS")
    saveRDS(results$tmat, "./tmat.RDS")
  }
  dmatrix <- assembleMatrix(results$dmat)
  tmatrix <- assembleMatrix(results$tmat)
  if (saveMatrixLists) {
    saveRDS(dmatrix, "./dmatrix.RDS")
    saveRDS(tmatrix, "./tmatrix.RDS")
  }
  return(list(dmatrix = dmatrix, tmatrix = tmatrix))
}
