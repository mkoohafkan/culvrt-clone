test_that("Velocity conversion is correct", {

  expect_equal(
    velocity_to_flow(2, 2),
    2 * pi,
    tolerance = 1e-5

  )
  expect_equal(
    velocity_to_flow(2, 1),
    0.5 * pi,
    tolerance = 1e-5

  )

  expect_equal(
    velocity_from_flow(20, 5),
    1.0186,
    tolerance = 1e-5
  )
})
