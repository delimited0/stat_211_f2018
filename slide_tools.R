#' incremental bullet
inc <- function(doc_type) {
  if (doc_type == "html") {
    "--"
  }
  else{
    ""
  }
}

#' new slide
ns <- function(doc_type) {
  if (doc_type == "html") {
    "---"
  }
  else{
    ""
  }
}