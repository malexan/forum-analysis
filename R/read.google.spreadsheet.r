#' Read table from Google Drive Spreadsheet.
#' 
#' @param gid Numeric: identificator of spreadsheet to read. Starting 
#' from 0, i.e. first sheet has gid 0, second - 1. Default is 0.
#' @param key String: key to access the document. Also user can set \code{.key}
#' variable. Available in online Google Drive Spreadsheet interface:
#' "File" - "Publish in Internet".
#' @import RCurl
#' @examples
#' .key <- 'bla-bla-bla'
#' read.google.spreadsheet()
#' 

read.google.spreadsheet <- function(gid = 0, key) {
  if(missing(key) & !exists('.key')) stop("\nDocument key is missing\n")
  if(missing(key)) key <- .key
  require(RCurl)
  url <- getURL(paste("https://docs.google.com/spreadsheet/pub?key=", key,
                      "&single=true&gid=", gid, "&output=csv", sep = ""), 
                .encoding = 'UTF-8', cainfo = system.file("CurlSSL", 
                                                          "cacert.pem", 
                                                          package = "RCurl"))
  read.csv(textConnection(url, encoding = 'UTF-8'), header = T, sep = ",")
}
