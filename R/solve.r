#' Solve for Culvert Velocity/Flow
#'
#' Root equation for solving culvert velocity or flow given a known
#' head loss. This function simply returns the difference of the
#' provided head loss and the evaluated result of [`loss_total`]
#' or [`loss_total_alt`]. These functions are designed to be used
#' in conjunction with [`stats::uniroot()`].
#'
#' @inheritParams loss_entrance
#' @inheritParams loss_total
#' @param head.loss Total head loss, ft.
#'
#' @export
root_velocity = function(v, head.loss, ...) {
  head.loss - loss_total(v = v, ...)
}

#' @rdname root_velocity
#'
#' @export
root_velocity_hist = function(v, head.loss, ...) {
  head.loss - loss_total_hist(v = v, ...)
}

#' @rdname root_velocity
#'
#' @export
root_velocity_alt = function(v, head.loss, ...) {
  head.loss - loss_total_alt(v = v, ...)
}


#' Solve for Culvert Gate Height
#'
#' Root equation for solving culvert gate height given a known
#' head loss and velocity. This function simply returns the
#' difference of the provided head loss and the evaluated result of
#' [`loss_total`] or [`loss_total_alt`]. These functions are designed
#' to be used in conjunction with [`stats::uniroot()`].
#'
#' @inheritParams root_velocity
#'
#' @export
root_height = function(h, head.loss, ...) {
  head.loss - loss_total(h = h, ...)
}

#' @rdname root_height
#' @export
root_height_hist = function(h, head.loss, ...) {
  head.loss - loss_total_hist(h = h, ...)
}

#' @rdname root_height
#' @export
root_height_alt = function(h, head.loss, ...) {
  head.loss - loss_total_alt(h = h, ...)
}
