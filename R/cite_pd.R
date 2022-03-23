#' Cite a reference using pandoc
#'
#' Unlike the normal use of pandoc to render `Rmd` to an output format, this
#' just returns a single formatted reference.
#'
#' This function can be used to generate pandoc-formatted references in formats
#' that normally don't support pandoc, such as slides with xaringan or
#' CSV files.
#'
#' The pandoc format for citing references is `@<key>`, where `<key>` is the
#' reference key (e.g., `@Nitta2011` or `[@Nitta2011]`).
#'
#' Note that since pandoc isn't seeing a full (R)md file, it won't
#' automatically disambiguate references (cases where an 'a', 'b', etc. needs
#' to be added to differentiate between publications whose author and
#' year are the same). If you need to distinguish between such publications
#' that are cited in the same file (or set of files), provide the file path(s)
#' to `context`.
#'
#' @param ref Character vector of length 1; a character string including
#' the reference to cite, in pandoc format. See examples.
#' @param bib Character vector; path to bibliography file(s) in `bib` or `yaml`
#' format.
#' @param csl Character vector of length 1; path to CSL file.
#' @param context Character vector; path to file(s) with citations in pandoc
#' format. These aren't cited, but used
#' for disambiguating references (see Details).
#' @param format Character vector of length 1; pandoc output format. Must be
#' a valid output format for the pandoc `-t` argument. For a complete list,
#' see https://pandoc.org/MANUAL.html#general-options
#' @param glue Logical vector of length 1; should the output be passed through
#' [glue::glue()]? Generally results in nicer printing (e.g., line breaks
#' show up on different lines instead of `\n`). Default `TRUE`.
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
#'   "https://raw.githubusercontent.com/citation-style-language/styles/master/ecology.csl", # nolint
#'   temp_csl
#' )
#'
#' # Cite the reference (note no disambiguation)
#' cite_pd("@Nitta2011a", my_bib, temp_csl)
#'
#' # Provide context for disambiguation: a CSV file with some cited references
#' csv_file <- system.file(
#'   "extdata", "data.csv", package = "rmdref", mustWork = TRUE)
#' read.csv(csv_file)
#'
#' cite_pd("@Nitta2011a", my_bib, temp_csl, csv_file)
#'
#' # Delete the temporary file
#' unlink(temp_csl)
#'
cite_pd <- function(
  ref, bib, csl, context = NULL,
  format = "plain", glue = TRUE) {

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

  # Format "no cite" list of citations
  # Normally used to include in bibliography without citing
  # But here, use for diambiguation
  no_cite <- NULL
  if (!is.null(context)) {
    # Extract references from context file,
    # include in "nocite" list in YAML header
    all_refs <- extract_citations(context)$key %>%
      unique() %>%
      sort() %>%
      paste0("@", .) %>%
      paste0(collapse = ", ") %>%
      paste0("  ", .)
    no_cite <- c("nocite: |", all_refs)
  }

  # Write out temporary md file
  temp_md <- fs::path_abs(tempfile(fileext = ".md"))
  on.exit(unlink(temp_md), add = TRUE)
  writeLines(c("---", "suppress-bibliography: true", no_cite, "---", ref), temp_md)

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

  if (isTRUE(glue)) return(glue::glue(result))

  result

}
