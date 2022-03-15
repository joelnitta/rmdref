#' Extract citations (formatted like `[@key]`) from an Rmd file
#'
#' @param rmd_file Character vector; path to Rmd file
#'
#' @return Dataframe (tibble) with one column 'key'
#' @export
#' @autoglobal
#' @examples
#' tempfile <- tempfile(fileext = ".Rmd")
#' lines <- c(
#'   "---",
#'   "title: 'Report'",
#'   "output_format: html_document",
#'   "---",
#'   "This statement is supported by @Nitta2021",
#'   "[@Foo1983; @Blah2001; but see @Smith2020]."
#' )
#' write(lines, tempfile)
#' extract_citations(tempfile)
#' file.remove(tempfile)
extract_citations <- function(rmd_file) {
  readr::read_lines(rmd_file) %>%
    stringr::str_split(" |;") %>%
    unlist %>%
    magrittr::extract(., stringr::str_detect(., "@")) %>%
    stringr::str_remove_all("^[^@]*") %>%
    stringr::str_remove_all('\\[|\\]|\\)|\\(|\\.$|,|\\{|\\}|\\\\|\\"|`') %>%
    magrittr::extract(., stringr::str_detect(., "^@|^-@")) %>%
    stringr::str_remove_all("^@|^-@") %>%
    unique %>%
    sort %>%
    tibble::tibble(key = .)
}
