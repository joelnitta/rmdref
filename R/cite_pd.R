#' Cite a reference using pandoc
#'
#' Unlike the normal use of pandoc to render `Rmd` to an output format, this
#' just returns a formatted citation or bibliography.
#'
#' `cite_pd()` creates citations; `bib_pd()` creates a bibliography
#' (list of references).
#'
#' This function can be used to generate pandoc-formatted references in formats
#' that normally don't support pandoc, such as slides with xaringan or
#' CSV files.
#'
#' The pandoc format for citing references is `@<key>`, where `<key>` is the
#' reference key (e.g., `@Nitta2011` or `[@Nitta2011]`).
#'
#' Note that since pandoc isn't seeing a full (R)md file, it won't automatically
#' disambiguate references when generating a citation (cases where an 'a', 'b',
#' etc. needs to be added to differentiate between publications whose author and
#' year are the same). If you need to distinguish between such publications that
#' are cited in the same file (or set of files), provide the file path(s) to
#' `context`.
#'
#' @param ref Character vector of length 1; a character string including
#' one or more citations in pandoc format used to create a citation
#' (`cite_pd()`) or bibliography entry (`bib_pd()`).
#' @param bib Character vector; path to bibliography file(s) in `bib` or `yaml`
#' format.
#' @param csl Character vector of length 1; path to CSL file.
#' @param context Character vector; path to file(s) with citations in pandoc
#' format. If used with `cite_pd()`, these aren't cited, but used
#' for disambiguating references (see Details). If used with `bib_pd()`, all
#' references in the file(s) will appear in the bibliography.
#' @param format Character vector of length 1; pandoc output format. Must be
#' a valid output format for the pandoc `-t` argument. For a complete list,
#' see https://pandoc.org/MANUAL.html#general-options
#' @param glue Logical vector of length 1; should the output be passed through
#' [glue::glue()]? Generally results in nicer printing (e.g., line breaks
#' show up on different lines instead of `\n`). Default `TRUE`.
#' @param wrap Character vector of length 1; type of line wrapping in output.
#'   With `auto` (the default), pandoc will attempt to wrap lines to the column
#'   width specified by `columns` (default 72). With `none`, pandoc will not
#'   wrap lines at all. With `preserve`, pandoc will attempt to preserve the
#'   wrapping from the source document.
#' @param columns Numeric vector of length 1; width of lines when wrapping.
#' @param other_pd_args Character vector; other arguments to pass to pandoc.
#' Must be formatted with one element per word
#' (as in `args` for [processx::run()]).
#' @param default_bib Name of a character vector object in the global
#' environment to use for `bib`; defaults to `"bib"`. Can be changed by
#' setting environmental variable `BIB_FILE`.
#' @param default_csl Name of a character vector object in the global
#' environment to use for `csl`; defaults to `"csl"`. Can be changed by
#' setting environmental variable `CSL_FILE`.
#' @param default_context Name of a character vector object in the global
#' environment to use for `context`; defaults to `"context"`. Can be changed by
#' setting environmental variable `CONTEXT_FILE`.
#'
#' @return Character vector.
#' @export
#' @examples
#' # Specify example bibliography file (yaml format)
#' bib <- system.file(
#'   "extdata", "refs.yaml", package = "rmdref", mustWork = TRUE)
#'
#' # Download a CSL file to a temporary location
#' csl <- tempfile(fileext = ".csl")
#' utils::download.file(
#'   "https://raw.githubusercontent.com/citation-style-language/styles/master/ecology.csl", # nolint
#'   csl
#' )
#'
#' # Cite the reference (note no disambiguation)
#' cite_pd("@Nitta2011a", bib = bib, csl = csl)
#'
#' # Since the `bib` and `csl` objects match their default names for `cite_pd()`
#' # they can be omitted from the function call.
#' cite_pd("@Nitta2011a")
#'
#' # Provide context for disambiguation: a CSV file with some cited references
#' csv_file <- system.file(
#'   "extdata", "data.csv", package = "rmdref", mustWork = TRUE)
#' read.csv(csv_file)
#'
#' cite_pd("@Nitta2011a", context = csv_file)
#'
#' # Make a list of references in the CSV file
#' bib_pd(context = csv_file, wrap = "none")
#'
#' # Delete the temporary file
#' unlink(csl)
#'
cite_pd <- function(
  ref, bib, csl, context = NULL,
  format = "plain", glue = TRUE,
  wrap = "auto", columns = 72,
  other_pd_args = NULL,
  default_bib = Sys.getenv("BIB_FILE", unset = "bib"),
  default_csl = Sys.getenv("CSL_FILE", unset = "csl"),
  default_context = Sys.getenv("CONTEXT_FILE", unset = "context")
  ) {

  # Use 'bib' in global env if available
  if (default_bib %in% ls(envir = .GlobalEnv) & missing(bib)) {
    bib <- get(default_bib, envir = .GlobalEnv)
  }
  # Use 'csl' in global env if available
  if (default_csl %in% ls(envir = .GlobalEnv) & missing(csl)) {
    csl <- get(default_csl, envir = .GlobalEnv)
  }
  # Use 'context' in global env if available
  if (default_context %in% ls(envir = .GlobalEnv) & missing(context)) {
    context <- get(default_context, envir = .GlobalEnv)
  }

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
  assertthat::assert_that(assertthat::is.string(wrap))
  assertthat::assert_that(assertthat::is.number(columns))
  if (!is.null(other_pd_args)) {
    assertthat::assert_that(is.character(other_pd_args))
  }

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
    "--wrap", wrap,
    "--columns", columns,
    # May include multiple bibliography files
    unlist(lapply(fs::path_abs(bib), function(x) c("--bibliography", x))),
    temp_md,
    other_pd_args
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

#' @rdname cite_pd
#' @export
bib_pd <- function(
  ref = NULL, bib, csl, context = NULL,
  format = "plain", glue = TRUE,
  wrap = "auto", columns = 72,
  other_pd_args = NULL,
  default_bib = Sys.getenv("BIB_FILE", unset = "bib"),
  default_csl = Sys.getenv("CSL_FILE", unset = "csl"),
  default_context = Sys.getenv("CONTEXT_FILE", unset = "context")) {

  # Use 'bib' in global env if available
  if (default_bib %in% ls(envir = .GlobalEnv) & missing(bib)) {
    bib <- get(default_bib, envir = .GlobalEnv)
  }
  # Use 'csl' in global env if available
  if (default_csl %in% ls(envir = .GlobalEnv) & missing(csl)) {
    csl <- get(default_csl, envir = .GlobalEnv)
  }
  # Use 'context' in global env if available
  if (default_context %in% ls(envir = .GlobalEnv) & missing(context)) {
    context <- get(default_context, envir = .GlobalEnv)
  }

  # Check input
  for (i in seq_along(bib)) {
    assertthat::assert_that(
      fs::file_exists(bib[[i]]),
      msg = glue::glue("Cannot find file '{bib[[i]]}'"))
  }
  assertthat::assert_that(
    fs::file_exists(csl),
    msg = glue::glue("Cannot find file '{csl}'"))
  assertthat::assert_that(assertthat::is.string(format))
  assertthat::assert_that(assertthat::is.flag(glue))
  assertthat::assert_that(assertthat::is.string(wrap))
  assertthat::assert_that(assertthat::is.number(columns))
  if (!is.null(other_pd_args)) {
    assertthat::assert_that(is.character(other_pd_args))
  }

  # Extract list of references from ref
  cited_refs <- NULL
  if (!is.null(ref)) {
    assertthat::assert_that(assertthat::is.string(ref))
    temp_txt <- fs::path_abs(tempfile(fileext = ".txt"))
    writeLines(ref, temp_txt)
    cited_refs <- extract_citations(temp_txt)$key %>%
      unique() %>%
      sort() %>%
      paste0("@", .)
  }

  # Extract list of references from context
  context_refs <- NULL
  if (!is.null(context)) {
    # Extract references from context file,
    # include in "nocite" list in YAML header
    context_refs <- extract_citations(context)$key %>%
      unique() %>%
      sort() %>%
      paste0("@", .)
  }

  if (is.null(ref) && is.null(context)) stop(
    "Must provide at least one of 'ref' or 'context'")

  # Combine into 'no_cite' list
  no_cite <-
    c(cited_refs, context_refs) %>%
    unique() %>%
    paste0(collapse = ", ") %>%
    paste0("  ", .) %>%
    c("nocite: |", .)

  # Write out temporary md file
  temp_md <- fs::path_abs(tempfile(fileext = ".md"))
  on.exit(unlink(temp_md), add = TRUE)
  writeLines(c("---", "suppress-bibliography: false", no_cite, "---"), temp_md)

  # Format args to pandoc
  args <- c(
    "--citeproc",
    "--to", format,
    "--csl", fs::path_abs(csl),
    "--wrap", wrap,
    "--columns", columns,
    # May include multiple bibliography files
    unlist(lapply(fs::path_abs(bib), function(x) c("--bibliography", x))),
    temp_md,
    other_pd_args
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
