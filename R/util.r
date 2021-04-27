#' Velocity/Flow Conversion
#'
#' Simple helper function to convert between flow and velocity.
#' These functions assume full flow through a circular pipe/culvert.
#'
#' @inheritParams loss_entrance
#' @inheritParams culvert_area
#' @return Fluid flow, ft^3/s.
#'
#' @details Flow is calculated as the product of velocity and culvert
#'   cross-sectional area.
#'
#' @export
velocity_to_flow = function(v, d = 5) {
  v * culvert_area(d, d)
}

#' @rdname velocity_to_flow
#' @param q Fluid flow, ft^3/s.
#' @return Fluid velocity, ft/s.
#'
#' @export
velocity_from_flow = function(q, d = 5) {
  q / culvert_area(d, d)
}
