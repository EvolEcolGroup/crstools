#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# create a dummy function that uses rlang
dummy_rlang <- function() {
  rlang::is_installed("ggplot2")
}