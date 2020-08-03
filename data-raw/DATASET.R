## code to prepare `DATASET` dataset goes here
library(tidyverse)
library(rvest)

tb <- "https://fileinfo.com/filetypes/common" %>%
  read_html() %>%
  html_table()
nms <- map(tb, colnames) %>%
  map_chr(1)
names(tb) <- nms
common_file_types <- tb %>%
  map(magrittr::set_colnames, c("ext", "desc")) %>%
  bind_rows(.id = "type") %>%
  mutate(ext = tolower(ext))


ctype <- unique(common_file_types$type)

stype <- c("text", "data", "audio", "video", "3d_img", "raster_img",
           "vector_img", "page_layout", "spreadsheet", "database", "executable",
           "game", "cad", "gis", "web", "plugin", "font", "system", "settings",
           "encoded", "compressd", "disk_img", "developer", "backup", "misc")

type_tojoin <- data.frame(type = ctype, stype = stype)


common_file_types <- full_join(common_file_types, type_tojoin) %>%
  as_tibble()

usethis::use_data(common_file_types, overwrite = TRUE)
