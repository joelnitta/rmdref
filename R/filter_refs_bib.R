#' Filter a list of references in a bibtex file to those occurring in an
#' Rmd file
#'
#' @param rmd_file Character vector; Path to Rmd file(s)
#' @param bib_in String or list; if string, the path to the bibtex file to
#' filter. If list, should be result of reading in a bibtex file with
#' [RefManageR::ReadBib()]
#' @param bib_out Path to write filtered bibtex file
#' @param silent Logical; should a warning be issued for missing references?
#'
#' @return NULL; externally, the filtered bibtex will be written to `bib_out`
#' @export
#' @examples
#' if (requireNamespace("bibtex", quietly = TRUE)) {
#'
#'   # Write out an example Rmd file
#'   rmd_tempfile <- tempfile(fileext = ".Rmd")
#'   lines <- c(
#'     "---",
#'     "title: 'Report'",
#'     "output_format: html_document",
#'     "---",
#'     "This statement is supported by @tekwe2013application",
#'     "[@sarkar2013adaptive]."
#'   )
#'   write(lines, rmd_tempfile)
#'
#'   # Filter the bibliography
#'   bib_tempfile <- tempfile(fileext = ".bib")
#'   filter_refs_bib(
#'     rmd_file = rmd_tempfile,
#'     bib_in = system.file("Bib", "RJC.bib", package = "RefManageR"),
#'     bib_out = bib_tempfile
#'   )
#'   # Check that it worked
#'   readLines(bib_tempfile)
#'
#'   # Cleanup
#'   file.remove(rmd_tempfile)
#'   file.remove(bib_tempfile)
#'
#' } else {
#'   warning("Need bibtex package for example")
#' }
filter_refs_bib <- function(
  rmd_file,
  bib_in = "main_library.bib",
  bib_out = "references.bib", silent = FALSE) {

  # Parse RMD file and extract citation keys
  citations <- purrr::map_df(rmd_file, extract_citations)

  # Read in bib including all references
  if (inherits(bib_in, "character")) {
    ref_bib <- RefManageR::ReadBib(bib_in)
  } else if (inherits(bib_in, "list")) {
    ref_bib <- bib_in
  } else {
    stop("`bib_in` must be a path to a bibtex file (string) or a list read in with RefManageR::ReadBib()") # nolint
  }

  # Extract all citation keys from full bib
  cite_keys_all <- names(ref_bib) %>%
    tibble::tibble(
      key = .,
      order = 1:length(.)
    )

  # Check that all keys in the yaml are present in the input YAML
  missing <- citations %>% dplyr::anti_join(cite_keys_all, by = "key")

  if (nrow(missing) > 0 && silent == FALSE)
    warning(glue::glue("The following ref keys are present in the Rmd but missing from the input bib file: {missing$key}")) # nolint

  # Filter bib to only those citation keys in the RMD
  ref_bib_filtered <- ref_bib[unique(citations$key)]

  # Write out the bib file
  RefManageR::WriteBib(bib = ref_bib_filtered, file = bib_out)

}
