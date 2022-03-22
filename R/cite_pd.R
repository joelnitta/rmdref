#' Cite a reference using pandoc
#'
#' Unlike the normal use of pandoc to render `Rmd` to an output format, this
#' just returns a single formatted reference.
#'
#' This function can be used to generate pandoc-formatted references in formats
#' that normally don't support pandoc, such as slides with xaringan or
#' CSV files.
#'
#' Note that since pandoc isn't seeing a full (R)md file, it won't
#' automatically disambiguate references (cases where an 'a', 'b', etc needs
#' to be added to differentiate between publications because the author and
#' year are the same). You can use the `del_after` argument as a work-around
#' (see Examples).
#'
#' @param ref Character vector of length 1; a character string including
#' the reference to cite, in pandoc MD format. See examples.
#' @param bib Character vector; path to bibliography file(s) in `bib` or `yaml`
#' format.
#' @param csl Character vector of length 1; path to CSL file.
#' @param format Character vector of length 1; pandoc output format. Must be
#' a valid output format for the pandoc `-t` argument. For a complete list,
#' see https://pandoc.org/MANUAL.html#general-options
#' @param glue Logical vector of length 1; should the output be passed through
#' [glue::glue()]?
#' @param del_after Character vector of length 1; string to mark place where
#' text will be deleted from that point to the end of the citation.
#'
#' @return Character vector.
#' @export
#' @examples
#' # Specify example bibliography file (yaml format)
#' my_bib <- system.file(
#'   "extdata", "refs.yaml", package = "rmdref", mustWork = TRUE)
#'
#' # Download a CSL file to a temporary location
#' temp_csl <- tempfile(fileext = ".csl")
#' utils::download.file(
#'   "https://raw.githubusercontent.com/citation-style-language/styles/master/ecology.csl",
#'   temp_csl
#' )
#'
#' # Cite the reference
#' cite_pd("@Nitta2011a", my_bib, temp_csl)
#'
#' # The first argument can be any string that may include one or more
#' # references in pandoc format
#' cite_pd(
#'   "I agree with @Nitta2021 because they are right [@Nitta2011b].",
#'   my_bib, temp_csl)
#'
#' # Use `del_after` as a work-around for disambiguating references.
#' # e.g., if you want to print "Nitta 2011a":
#' cite_pd("@Nitta2011a|@Nitta2011b", my_bib, temp_csl, del_after = "|")
#'
#' # Delete the temporary file
#' unlink(temp_csl)
#'
cite_pd <- function(
  ref, bib, csl, format = "plain",
  glue = TRUE, del_after = NULL) {

  # Check input
  for (i in seq_along(bib)) {
    assertthat::assert_that(
      fs::file_exists(bib[[i]]),
      msg = glue::glue("Cannot find file '{bib[[i]]}'"))
  }
  assertthat::assert_that(
    fs::file_exists(csl),
    msg = glue::glue("Cannot find file '{csl}'"))
  assertthat::assert_that(assertthat::is.string(ref))
  assertthat::assert_that(assertthat::is.string(format))
  assertthat::assert_that(assertthat::is.flag(glue))

  # Write out temporary md file
  temp_md <- fs::path_abs(tempfile(fileext = ".md"))
  on.exit(unlink(temp_md), add = TRUE)
  writeLines(c("---", "suppress-bibliography: true", "---", ref), temp_md)

  # Format args to pandoc
  args <- c(
    "--citeproc",
    "--to", format,
    "--csl", fs::path_abs(csl),
    # May include multiple bibliography files
    unlist(lapply(fs::path_abs(bib), function(x) c("--bibliography", x))),
    temp_md
  )

  # Run pandoc
  result <- processx::run(
    rmarkdown:::pandoc(),
    args
  )$stdout

  Encoding(result) <- "UTF-8"

  # Optionally delete text at end
  if (!is.null(del_after)) {
    assertthat::assert_that(assertthat::is.string(del_after))
    result <- stringr::str_remove_all(result, glue::glue("\\{del_after}.*$"))
  }

  if (isTRUE(glue)) return(glue::glue(result))

  result

}
