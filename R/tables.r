#' Slide Gate Head Loss Tables
#'
#' Tabulated coefficients for selected gate opening fractions.
#'
#' @name gate-loss
#' @return A two-column `data.frame`. First column is
#'   the gate opening height to culvert diameter ratio \eqn{h / d},
#'   Second column is the coefficient value.
NULL

#' @rdname gate-loss
#' @details [`usda_ks()`] uses `ks` values from the USDA W.E.S. Hyd.
#'   Chart 330-1, "Gate headloss coefficient (Kg) full pipe flow"
#'   (USDA, 1969, p. B-25).
#'
#' @export
usda_ks = function() {
  rrds.ks.table
}

#' @rdname gate-loss
#' @details [`brater_ks()`] uses `ks` values from Brater and King
#'   "Handbook of hydraulics for the solution of hydraulic engineering
#'   problems" (1976).
#'
#' @export
brater_ks = function() {
  data.frame(
    open_fraction = c(0.125, 0.25, 0.375, 0.5, 0.75, 1.0),
    coefficient = c(30, 10, 4.5, 2, 1.5, 0.9)
  )
}

#' Culvert Obstruction Loss Coefficients
#'
#' Tabulated coefficients for selected ratios of total culvert area to
#'   unobstructed area.
#'
#' @name obstruction-loss
#' @return A two-column `data.frame`. First column is
#'   the total culvert area to unobstructed area ratio \eqn{A / A_o},
#'   Second column is the coefficient value.
NULL

#' @rdname obstruction-loss
#' @details [`king_ko()`] uses `ko` values from the National Hydraulic ###
#'
#' @export
king_ko = function() {
  data.frame(
    area_ratio = c(1.0, 1.05, 1.1, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.5,
      3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0),
    coefficient = c(0.0, 0.1, 0.19, 0.42, 0.96, 1.54, 2.17, 2.70,
      3.27, 4.0, 5.06, 6.75, 8.01, 9.4, 10.4, 11.3, 12.5, 13.5)
  )
}