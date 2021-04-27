test_that("Velocity solver works", {
  d = 5
  l = 98.5
  n = 0.01
  g = 32.2

  expect_equal(
    root_velocity(v = 1.5, head.loss = 0.2764,
      h = 2.5, d = d, l = l, ke = 0.9, ku = 29,
      n = n, ks.table = usda_ks(),
      alpha = 1, g = g),
    0.0,
    tolerance = 1e-5
  )
})





test_that("Alternate velocity solver works", {
  d = 5
  l = 95.5
  g = 32.16
  n = 0.01
  # f/d ~ ku * n^2 / (rh^1.33)
  # ku ~ f * (rh^1.33) / (d * n^2)
  foverd = 0.0729 / d
  rh = culvert_hydraulic_radius(d, d)
  eq.ku = foverd * (rh ^ (4 / 3)) / (n ^ 2)

  expect_equal(
    root_velocity_hist(v = 1.937, head.loss = 2,
      h = 0.625, d = d, l = l, ke = 0.9, ku = eq.ku,
      n = n, ks.table = brater_ks(),
      alpha = 1, g = g),
    0.0,
    tolerance = 1e-3
  )

  expect_equal(
    uniroot(root_velocity_hist, c(0, 100), head.loss = 2.0,
      h = 0.625, d = d, l = l, ke = 0.9, ku = eq.ku,
      n = n, ks.table = brater_ks(),
      alpha = 1, g = g)$root,
    1.937,
    tolerance = 1e-3
  )
})



