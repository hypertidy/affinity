#' Adjacency
#'
#' Functions 'bottom left', 'top left', 'bottom right', and 'top right' named by
#' their initials, provide very low level relative positional structures for use in
#' raster logic.
#'
#' Some tiny functions 'image0', 'image1', 'text0' exist purely to illustrate the ideas in
#' a vignette.
#' @export
#' @name adjacencies
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
#' @export
#' @name adjacencies
tl <-  function(x) {
  ## top right
  cbind(NA_integer_, rbind(x, NA_integer_))
}
#' @export
#' @name adjacencies
br <- function(x) {
  ## bottom left
  cbind(rbind(NA_integer_, x), NA_integer_)
}
#' @export
#' @name adjacencies
tr <- function(x) {
  ## bottom right
  cbind(rbind(x, NA_integer_), NA_integer_)
}

#' @export
#' @name adjacencies
#' @param x matrix to plot
#' @param ... arguments passed to image()
image0 <- function(x, ...) image(seq(0, nrow(x)), seq(0, ncol(x)), x, ...)
#' @export
#' @name adjacencies
#' @importFrom graphics image text
image1 <- function(x, ...) image(seq(1, nrow(x) + 1), seq(1, ncol(x) + 1), x, ...)
#' @export
#' @name adjacencies
text0 <- function(x, ...) {
  text(expand.grid(seq(0.5, by = 1, length.out = nrow(x)),
                   seq(0.5, by = 1, length.out = nrow(x))), lab = x,
       ...)
}
