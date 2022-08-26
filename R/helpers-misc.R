#' @title Extracts the extension of a file name
#'
#' @details Extracts the file extension from a character string using //. as the
#'   separator.
#'
#' @param file_name Character string of a file name or path. Must only include
#'   one ".", which is used as the separator.
#'
#' @importFrom tidyr separate

extract_file_extension <- function(file_name){
  extension <- file_name %>%
    data.frame() %>%
    separate(col = 1, into = c(NA, "EXT"), sep = "\\.")

  extension$EXT
}

