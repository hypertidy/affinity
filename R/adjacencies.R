#' Adjacency
#'
#'
#' @noRd
#' @examples
#' (m <- matrix(1:12, 3))
#' tl(m)
#' tr(m)
#' bl(m)
#' br(m)
#' tl(br(m))
#' image0(tl(br(m)))
#' text0(tl(br(m)))
bl <- function(x) {
  ## top left
  cbind(NA_integer_, rbind(NA_integer_, x))
}

tl <-  function(x) {
  ## top right
  cbind(NA_integer_, rbind(x, NA_integer_))
}
br <- function(x) {
  ## bottom left
  cbind(rbind(NA_integer_, x), NA_integer_)
}
tr <- function(x) {
  ## bottom right
  cbind(rbind(x, NA_integer_), NA_integer_)
}
image0 <- function(x, ...) image(seq(0, nrow(x)), seq(0, ncol(x)), x, ...)
image1 <- function(x, ...) image(seq(1, nrow(x) + 1), seq(1, ncol(x) + 1), x, ...)
text0 <- function(x, ...) {
  text(expand.grid(seq(0.5, by = 1, length.out = nrow(x)),
                   seq(0.5, by = 1, length.out = nrow(x))), lab = x,
       ...)
}
