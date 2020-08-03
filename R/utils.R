

#' @importFrom httr GET timeout
try_GET <- function (x, ...) {
  tryCatch(
    GET(url = x, timeout(10), ...),
    error = function(err) conditionMessage(err),
    warning = function(warn) conditionMessage(warn))
}

is_response <- function (x) {
  class(x) == "response"
}

# string cleaning ---------------------------------------------------------


backticks <- function(x) {
  paste0("`", x, "`", collapse = ",")
}

clean_query <- function(x) {
  gsub("\\?.*", "", x)
}

is_url <- function(x) {
  grepl("(https|http)",x)
}

file_ext <- function (x) {
  x <- clean_query(x)
  idx <- regexpr("\\.([[:alnum:]]+)$", x)
  pos <- ifelse(idx > -1L, substring(x, idx + 1L), "")
  out <- ifelse(is_url(x), "", pos) # remove
  ifelse(out == "", NA_character_, paste0(".", out))
}

file_ext_desc <- function(x) {
  dm <- match(x, get("common_file_types")$ext)
  get("common_file_types")$desc[dm]
}

file_ext_category <- function(x) {
  cat <- match(x, get("common_file_types")$ext)
  get("common_file_types")$type[cat]
}

