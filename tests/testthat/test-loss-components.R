test_that("Head loss components are correct", {
  v = 1.937
  h = 0.625
  d = 5
  l = 95.5
  g = 32.16
  n = 0.01
  ku = 29.164
  # f/d ~ ku * n^2 / (rh^1.33)
  # n ~ sqrt(f * (rh^1.33) / (d * ku))
  foverd = 0.0729 / d
  rh = culvert_hydraulic_radius(d, d)
  eq.n = sqrt(foverd * (rh ^ (4 / 3)) / ku)

  # entrance loss
  expect_equal(
    loss_entrance(v, 0.9, g),
    0.052,
    tolerance = 1e-3
  )
  # outlet loss
  expect_equal(
    loss_outlet(v, 0, 1.0, g),
    0.058,
    tolerance = 1e-3
  )
  # gate loss (brater)
  expect_equal(
    loss_slide(v, h, d, brater_ks(), g),
    1.75,
    tolerance = 1e-5
  )
  # gate loss (USDA)
  expect_equal(
    loss_slide(v, 2.5, d, usda_ks(), g),
    0.1225,
    tolerance = 1e-5
  )
  # pipe friction loss
  expect_equal(
    loss_pipe(v, d, l, 29.164, eq.n, g),
    0.081,
    tolerance = 1e-3
  )

  expect_equal(
    loss_pipe(2.5, d, l, 29.164, n, g),
    0.020,
    tolerance = 1e-4
  )
})
