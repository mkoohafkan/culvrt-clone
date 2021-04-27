
#' Culvert Hydraulic Geometry
#'
#' Functions for calculating culvert flow area, wetted perimeter, and
#' hydraulic radius.
#'
#' @param h Slide gate height, ft.
#' @param d Culvert diameter, ft. Default is 5 ft.
#' @param leaf The leaf type, either `"circular"` or `"square"`.
#' @return Culvert area, ft^2.
#'
#' @export
culvert_area = function(h, d = 5, leaf = c("square", "circular")) {
  leaf = match.arg(leaf, c("square", "circular"))
  if (isTRUE(all.equal(h, d))) {
    pi * (0.5 * d) ^ 2
  }
  else if (leaf == "square") {
    r = 0.5 * d
    r * r * acos(1 - h / r) - (r - h) * sqrt(2 * r * h - h * h)
  } else {
    # TODO
  }
}

#' @rdname culvert_area
#'
#' @return Wetted perimeter, ft.
#'
#' @importFrom stats uniroot
#' @export
culvert_wetted_perimeter = function(h, d = 5,
  leaf = c("square", "circular")) {
  leaf = match.arg(leaf, c("square", "circular"))
  if (isTRUE(all.equal(h, d))) {
    pi * d
  } else if (leaf == "square") {
    afun = function(theta, r, a) {
      a - 0.5 * (theta - sin(theta)) * r * r
    }
    theta = uniroot(afun, c(0, 2 * pi), r = 0.5 * d,
      a = culvert_area(h, d, leaf))$root
    0.5 * d * theta
  } else {
    # TODO
    stop("Circular leaf is not supported yet.")
  }
}


#' @rdname culvert_area
#'
#' @return Hydraulic radius, ft.
#'
#' @export
culvert_hydraulic_radius = function(h, d,
  leaf = c("square", "circular")) {
  culvert_area(h, d, leaf) / culvert_wetted_perimeter(h, d, leaf)
}
