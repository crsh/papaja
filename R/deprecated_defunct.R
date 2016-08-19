#' @export

apa_prepare_doc <- function(x) {
  .Deprecated(msg = "'apa_prepare_doc' is deprecated as of version 0.1.0.9423 because it is no longer needed. You may remove it from your manuscript without consequences.")
  return(invisible())
}


#' @export

fetch_web_refs <- function(x) {
  .Defunct("downloader::download")
  return(invisible())
}
