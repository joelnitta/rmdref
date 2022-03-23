#' Cite reference(s) in a bibtex file
#'
#' The bibliography should be read in from a `bibtex` file with
#' [RefManageR::ReadBib()]
#'
#' Either `bib` or `default_bib` should be provided, but not both
#' (see examples).
#'
#' `cite_p()` is a shortcut for `cite_bib(type = "p", ...)` and
#' `cite_t()` is a shortcut for `cite_bib(type = "t", ...)`.
#'
#' @param key Character vector; reference bibtex key(s)
#' @param type Character vector of length 1; type of citation. Must choose
#' "t" (textual citation) or "p" (parenthetical citation)
#' @param bib List of class 'bibentry'; bibliography read in from a bibtex file
#' @param default_bib Name of a bibliography object (list of class 'bibentry')
#' in the global environment to use instead of `bib`; defaults to "bib"
#'
#' @return Character vector of length 1; the formatted reference.
#' @export
#'
#' @examples
#' # Load a bibliography
#' if (requireNamespace("bibtex", quietly = TRUE)) {
#' file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
#' bib <- RefManageR::ReadBib(file.name)
#'
#' # Check some of the keys (the name of each entry is its bibtex key)
#' head(names(bib))
#'
#' # If an object called "bib" exists, it will be used by default for citing
#' cite_bib(c("tekwe2013application", "sarkar2013adaptive"), "t")
#' # You can change the default name with the environmental variable "BIB_NAME"
#' my_bib <- bib
#' rm(bib)
#' Sys.setenv("BIB_NAME" = "my_bib")
#' cite_bib("tekwe2013application", "t")
#' # Or provide the bibliography directly
#' cite_bib("tekwe2013application", "t", bib = my_bib)
#'
#' # Use cite_t() and cite_p() to avoid specifying "type"
#' cite_p("tekwe2013application")
#' cite_t("tekwe2013application")
#' } else {
#'   warning("Need bibtex package for example")
#' }
cite_bib <- function(
  key, type = c("t", "p"), bib,
  default_bib = Sys.getenv("BIB_NAME", unset = "bib")) {
  # Use 'bib' in global env if available
  if (default_bib %in% ls(envir = .GlobalEnv) & missing(bib)) {
    bib <- get(default_bib, envir = .GlobalEnv)
  }
  # Check input
  assertthat::assert_that(!is.null(bib), msg = "Must provide 'bib'")
  assertthat::assert_that(
    inherits(bib, "BibEntry"),
    msg = "'bib' must be of class 'BibEntry'"
  )
  assertthat::assert_that(is.character(key))
  assertthat::assert_that(assertthat::is.string(type))
  # Check keys
  key <- stringr::str_remove_all(key, "@")
  bad_keys <- setdiff(key, names(bib))
  assertthat::assert_that(
    length(bad_keys) == 0,
    msg =  glue::glue("Following key(s) missing from bib: {paste0(bad_keys, collapse = ', ')}")
  )
  # Cite
  switch(
    type,
    "t" = return(RefManageR::Citet(bib[key])),
    "p" = return(RefManageR::Citep(bib[key])),
    "'type' must be 't' or 'p'"
  )
}

#' @rdname cite_bib
#' @export
cite_p <- function(
  key, bib, default_bib = Sys.getenv("BIB_NAME", unset = "bib")) {
  cite_bib(key = key, bib = bib, type = "p", default_bib = default_bib)
}

#' @rdname cite_bib
#' @export
cite_t <- function(
  key, bib, default_bib = Sys.getenv("BIB_NAME", unset = "bib")) {
  cite_bib(key = key, bib = bib, type = "t", default_bib = default_bib)
}
