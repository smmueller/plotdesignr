# get_utm -----

test_that('Returns correct utm', {
  expect_equal(get_utm(-122), 10)
  expect_equal(get_utm(151), 56)
  expect_equal(get_utm(-86), 16)
})


