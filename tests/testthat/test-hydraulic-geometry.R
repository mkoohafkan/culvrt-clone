test_that("Culvert area is correct", {
  expect_equal(
    culvert_area(2, 2),
    pi
  )
  expect_equal(
    culvert_area(1, 2, "square"),
    0.5 * pi
  )
})

test_that("Culvert wetted perimeter is correct", {
  expect_equal(
    culvert_wetted_perimeter(2, 2),
    2 * pi
  )
  expect_equal(
    culvert_wetted_perimeter(1, 2),
    pi
  )
})

test_that("Culvert hydraulic radius is correct", {
  expect_equal(
    culvert_hydraulic_radius(2, 2),
    0.5
  )
  expect_equal(
    culvert_hydraulic_radius(1, 2),
    0.5
  )
})
