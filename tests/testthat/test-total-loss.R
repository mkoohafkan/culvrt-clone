test_that("Loss formula is correct", {
  v = 1.937
  h = 0.625
  d = 5
  l = 98.5
  g = 32.2
  n = 0.01

  expect_equal(
    loss_total(v = 1.5, h = 2.5, d = d,
      l = l, ke = 0.9, ku = 29, n = n, ks.table = usda_ks(),
      alpha = 1, g = g),
    0.2764,
    tolerance = 1e-4
  )
})


test_that("Historical Loss formula is correct", {
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
    loss_total_hist(v = velocity_from_flow(8.504), h = 0.625, d = d,
      l = l, ke = 0.9, ku = eq.ku, n = n, ks.table = brater_ks(),
      alpha = 1, g = g),
    0.1,
    tolerance = 1e-5
  )

  expect_equal(
    loss_total_hist(v = 1.937, h = 0.625, d = d, l = l,
      ke = 0.9, ku = eq.ku, n = n, ks.table = brater_ks(),
      alpha = 1, g = g),
    2.0,
    tolerance = 1e-3)

  expect_equal(
    loss_total_hist(v = velocity_from_flow(46.576), h = 0.625, d = d,
      l = l, ke = 0.9, ku = eq.ku, n = n, ks.table = brater_ks(),
      alpha = 1, g = g),
    3.0,
    tolerance = 1e-4
  )

  expect_equal(
    loss_total_hist(v = velocity_from_flow(46.266), h = 3.75, d = d,
      l = l, ke = 0.9, ku = eq.ku, n = n, ks.table = brater_ks(),
      alpha = 1, g = g),
    0.5,
    tolerance = 1e-4
  )
})
