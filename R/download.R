

#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom curl has_internet
#' @importFrom httr http_error message_for_status
url_content_a <- function(url) {

  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }
  resp <- try_GET(url)
  if (!is_response(resp)) {
    message(resp)
    return(invisible(NULL))
  }
  if (httr::http_error(resp)) {
    httr::message_for_status(resp)
    return(invisible(NULL))
  }
  html_nodes(read_html(resp), "a")
}


# List content of a page --------------------------------------------------

#' List all the contents of a webpage
#'
#' @param url the url.
#' @param pattern the extension.
#' @param full whether to display the relative path of the absolute path.
#' @param negate_expr exclude some pages that do not have usefull content.
#'
#' @importFrom rvest html_attr html_text
#' @export
list_content <- function(url, pattern = NULL, full = FALSE,
                         negate_expr = c("[^#|^#0-9^/||^/A-Za-z+$]")) {
  url_content <- url_content_a(url)
  description <- html_text(url_content, "href")
  filename <- html_attr(url_content, "href")
  if(!is.null(negate_expr)) {
    keep_expr <- grepl(negate_expr, filename)
    filename <- filename[keep_expr]
    description <- description[keep_expr]
  }
  if(!is.null(pattern)) {
    keep_pattern <- grep(pattern, filename)
    filename <- filename[keep_pattern]
    description <- description[keep_pattern]
  }
  if(isTRUE(full)) {
    filename <- paste0(url, filename)
  }
  extension <- file_ext(filename)
  extension_desciption <- file_ext_desc(extension)
  extension_type <- file_ext_category(extension)

  out <- tibble(
    Filename = filename,
    Description = description,
    Extension = extension,
    `Extension-Description` = extension_desciption,
    `Extension-Type` = extension_type
  )
  class(out) <- append("content", class(out))
  out
}


#' @importFrom cli cat_line
#' @export
print.content <- function(x, ..., n = nrow(x), width = NULL, n_extra = NULL) {
  cli::cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
  invisible(x)
}

# Download helpers --------------------------------------------------------


#' Ask to input the extension type
#'
#' @export
ask <- function() {
  readline(prompt = "Enter file extension (e.g. .pdf): \n ")
}


#' Helper function to split the file extensions into type
#'
#' @param type category of file extension
#'
#' @export
file_type <- function(type) {
  stype <- c("text", "data", "audio", "video", "3d_img", "raster_img",
             "vector_img", "page_layout", "spreadsheet", "database", "executable",
             "game", "cad", "gis", "web", "plugin", "font", "system", "settings",
             "encoded", "compressd", "disk_img", "developer", "backup", "misc")
  if(!type %in% stype) {
    stop("Wrong file type.", call. = TRUE)
  }
  sub <- type == get("common_file_types")$stype
  get("common_file_types")[sub, ]$ext
}


# Download ----------------------------------------------------------------

#' @importFrom xml2 url_absolute
build_url <- function(url, pattern) {
  relative_url_raw <- html_attr(url_content_a(url), "href")
  keep_ext <- grep(pattern, file_ext(relative_url_raw))
  relative_url <- relative_url_raw[keep_ext]
  url_absolute(relative_url, url)
}

path_file <- function (path) {
  is_missing <- is.na(path)
  path[!is_missing] <- basename(path[!is_missing])
  as.character(path)
}

build_filename <- function(url_abs, dest = ".") {
  path_end <- gsub(".*/(.+)", "\\1", url_abs) # maybe path_file
  clean_path <- clean_query(path_end)

  # Creat new dir if it doesn't exist
  if(!dir.exists(dest)) {
    dir.create(dest)
  }
  file.path(dest, clean_path)
}


#' Download multiple file from a url
#'
#'
#' @inheritParams list_content
#' @param ext ask() or file_type()
#' @param dest a character string (or vector, see url) with the name where the
#' downloaded file is saved. Tilde-expansion is performed.
#' @param verbose whether to print the url of the data.
#' @param ... further options.
#'
#' @importFrom httr GET write_disk
#' @importFrom tibble tibble
#' @export
download <- function(url, ext = ask(), dest = ".", verbose = TRUE, ...) {

  valid_ext <- ext %in% get("common_file_types")$ext
  if(!all(valid_ext)) {
    stop("Invalid file extension. Do not recognise ",
         backticks(ext[!valid_ext]), " format.", call. = FALSE)
  }
  if (length(url) != 1L || typeof(url) != "character") {
    stop("'url' must be a length-one character vector", call. = FALSE)
  }
  if (length(dest) != 1L || typeof(dest) != "character") {
    stop("'dest' must be a length-one character vector", call. = FALSE)
  }
  pattern_ext <- paste(ext, collapse = "|")
  file_urls <- build_url(url, pattern_ext)
  file_names <- build_filename(file_urls, dest)
  n_files <- length(file_names)

  if(n_files == 0) {
    stop("No files available with this extension.", call. = FALSE)
  }
  if (interactive() && verbose) {
    message("Accessing: ", url, "\nDownloading: ", n_files, " file(s).")
  }

  for(i in 1:length(file_names)) {
    resp <- GET(file_urls[i], write_disk(file_names[i], ...))
    # This chunk is just to make sure that the files are accessible
    if (!is_response(resp)) {
      message(resp)
      return(invisible(NULL))
    }
  }
}


