#' Entrance Head Loss
#'
#' Culvert entrance head loss
#'
#' @param v Fluid velocity, ft/s
#' @param ke Entrance loss coefficient. Default is 0.9 (USDOT 2005).
#' @param g Gravitational acceleration. Default value is 32.2 ft/s^2.
#' @return The entrance head loss, ft.
#'
#' @export
loss_entrance = function(v, ke = 0.9, g = 32.2) {
  0.5 * ke * v * v / g
}

#' Pipe Friction Head Loss
#'
#' Friction head loss from pipe geometry and surface roughness.
#'
#' @inheritParams loss_entrance
#' @param d Pipe diameter, ft. Default value is 5 ft.
#' @param l Pipe length, ft. Default value is 98.5 ft.
#' @param ku Loss coefficient. Default value is 29.
#' @param n Mannings roughness coeffient. Default value is 0.1.
#' @return Pipe friction head loss, ft.
#'
#' @export
loss_pipe = function(v, d = 5, l = 98.5, ku = 29.164, n = 0.022,
  g = 32.2) {
  # hydraulic radius, assumes total submergence
  r_hyd = culvert_hydraulic_radius(d, d)
  kc = ku * (n ^ 2) / (r_hyd ^ (4 / 3))
  loss_entrance(v, kc * l, g)
}

#' Slide Gate Head Loss
#'
#' Culvert slide gate head loss
#'
#' @inheritParams loss_pipe
#' @param h Slide gate opening, ft.
#' @param ks.table Slide gate head loss table. Used to determine the
#'    value of `ks` as a function of the ratio of the slide gate
#'    opening height to pipe diameter. Default uses the built-in
#'    [`usda_ks()`] lookup table.
#' @return The entrance head loss, ft.
#'
#' @details Coefficient values are based on a linear interpolation
#'   of values specified in `ks.table`. The nearest (i.e., minimum or
#'   maximum) coefficient value is used for gate opening values outside
#'   the range specified in the table.
#'
#' @seealso [`usda_ks()`] [`brater_ks()`]
#'
#' @importFrom stats approx
#' @export
loss_slide = function(v, h, d = 5, ks.table = usda_ks(), g = 32.2) {
  # height ratio
  dr = h / d
  ks = approx(x = ks.table, xout = dr, rule = 2L)$y
  loss_entrance(v, ks, g)
}

#' Outlet Head Loss
#'
#' Culvert outlet head loss.
#'
#' @inheritParams loss_entrance
#' @param vd Downstream velocity, ft/s. Default value is 0.
#' @param alpha Outlet coefficient. Default value is 1.
#' @return Outlet head loss, ft.
#'
#' @export
loss_outlet = function(v, vd = 0, alpha = 1, g = 32.2) {
  0.5 * alpha * ((v * v) - (vd * vd)) / g
}

#' Flap Gate Head Loss
#'
#' Head loss at flap gate.
#
#' @inheritParams loss_pipe
#' @param kf Flap gate weight correction factor. Default value is 8.
#' @return Flap gate head loss, ft.
#'
#' @export
loss_flap = function(v, d = 5, kf = 8, g = 32.2) {
  ## Is d the culvert diameter??
  coeff = kf * exp(-1.15 * v / sqrt(d))
  loss_entrance(v, coeff, g)

}

#' Head Loss From Obstruction
#'
#' Head loss resulting from obstruction inside pipe, e.g., a
#' partially-closed valve. Provides an alternative approach to
#' Computing slide gate loss.
#'
#' @details This approach may be preferable to [`loss_slide()`] because
#'   this equation assumes the obstruction is inside the pipe, whereas
#'   the slide gate loss equation assumes the slide gate is at the
#'   pipe entrance. Because the Roaring River Distribution System
#'   culvert slide gates are inside the pipes, the obstruction loss
#'   equation may be a better approximation of the loss.
#'
#' @inheritParams loss_slide
#' @param ko.table Obstruction head loss table. Used to determine the
#'    value of `ko` as a function of the slide gate opening height.
#'    Default uses the built-in [`king_ko()`] lookup table.
#' @param ... Additional arguments to [`culvert_area()`].
#
#' @importFrom stats approx
#' @export
loss_obstruction = function(v, h, d = 5, ko.table = king_ko(),
  g = 32.2, ...) {
  # area around obstruction
  ao = culvert_area(h, d, ...)
  # full area
  a = culvert_area(d, d)
  # area ratio for use with ko.table
  ar = a / ao
  ko = approx(ko.table, xout = ar, rule = 2L)$y
  loss_entrance(v, ko, g)
}

#' Total Head Loss
#'
#' Total head loss, i.e., combined entrance, slide gate/obstruction,
#' pipe friction, outlet, and junction chamber losses.
#'
#' @details the total head loss formula is
#'   \deqn{
#'     L_{total} = L_{entrance} + L_{pipe friction} + L_{slide gate} +
#'       L_{outlet} + L_{flap gate}
#'   }
#'   where \eqn{L_{outlet}} accounts for  junction chamber connection
#'   and \eqn{L_{flap gate}} accounts for the culvert outlet.
#'   Note that the total head loss equation was originally designed
#'   to use \eqn{ks} values from `usda_ks()` for computing slide gate
#'   losses.
#'
#' @param ... Arguments to the component loss functions, i.e.,
#'   [`loss_entrance()`], [`loss_slide()`], [`loss_obstruction()`],
#'   [`loss_pipe()`], [`loss_outlet()`], and [`loss_flap()`].
#' @return Total head loss, ft.
#'
#' @export
loss_total = function(...) {

  l = list(...)

  entrance.loss = do.call(loss_entrance,
    l[intersect(names(l), names(formals(loss_entrance)))])

  pipe.loss = do.call(loss_pipe,
    l[intersect(names(l), names(formals(loss_pipe)))])

  slide.loss = do.call(loss_slide,
    l[intersect(names(l), names(formals(loss_slide)))])

  outlet.loss = do.call(loss_outlet,
    l[intersect(names(l), names(formals(loss_outlet)))])

  flap.loss = do.call(loss_flap,
    l[intersect(names(l), names(formals(loss_flap)))])

  entrance.loss + pipe.loss + slide.loss + outlet.loss + flap.loss
}

#' @rdname loss_total
#'
#' @details the historical total head loss formula is
#'   \deqn{
#'     L_{total} = L_{entrance} + L_{pipe friction} + L_{slide gate} +
#'     2L_{outlet}
#'   }
#'   where the same outlet loss equation is used for both the junction
#'   chamber connection and the culvert outlet.
#'   Note that the historical total head loss equation was originally
#'   designed to use \eqn{ks} values from `brater_ks()` for computing
#'   slide gate losses. Also, the outlet loss term was originally
#'   designed using an alternative parameterization of friction loss
#'   which is approximately equivalent to an `n` value of 0.026.
#'
#' @export
loss_total_hist = function(...) {

  l = list(...)

  entrance.loss = do.call(loss_entrance,
    l[intersect(names(l), names(formals(loss_entrance)))])

  pipe.loss = do.call(loss_pipe,
    l[intersect(names(l), names(formals(loss_pipe)))])

  slide.loss = do.call(loss_slide,
    l[intersect(names(l), names(formals(loss_slide)))])

  outlet.loss = do.call(loss_outlet,
    l[intersect(names(l), names(formals(loss_outlet)))])


  entrance.loss + pipe.loss + slide.loss + 2 * outlet.loss
}


#' @rdname loss_total
#'
#' @details the alternate total head loss formula is
#'   \deqn{
#'     L_{total} = L_{entrance} + L_{pipe friction} + L_{obstruction} +
#'       L_{outlet} + L_{flap gate}
#'   }
#'   where the obstruction loss term replaces the slide gate loss term.
#'
#' @export
loss_total_alt = function(...) {

  l = list(...)

  entrance.loss = do.call(loss_entrance,
    l[intersect(names(l), names(formals(loss_entrance)))])

  pipe.loss = do.call(loss_pipe,
    l[intersect(names(l), names(formals(loss_pipe)))])

  obstruction.loss = do.call(loss_obstruction,
    l[intersect(names(l), names(formals(loss_obstruction)))])

  outlet.loss = do.call(loss_outlet,
    l[intersect(names(l), names(formals(loss_outlet)))])

  flap.loss = do.call(loss_flap,
    l[intersect(names(l), names(formals(loss_flap)))])

  entrance.loss + pipe.loss + obstruction.loss + outlet.loss + flap.loss
}
