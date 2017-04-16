context("flavornet")

test_that("fn_percept()", {
  skip_on_cran()

  a <- fn_percept("123-32-0")
  b <- fn_percept(c("75-07-0", "123-32-0"))
  c <- fn_percept(c("75-07-0", "123-32-0", "50-00-0"))

  expect_is(a, 'character')
  expect_is(b, 'character')
  expect_is(c, 'character')
  expect_equal(length(a), 1)
  expect_equal(length(b), 2)
  expect_equal(length(c), 3)

  expect_equal(a, structure("cocoa, roasted nut, roast beef, medicine", .Names = "123-32-0"))
  expect_equal(b, structure(c("pungent, ether", "cocoa, roasted nut, roast beef, medicine"
  ), .Names = c("75-07-0", "123-32-0")))
  expect_true(is.na(c[[3]]))

  expect_warning(fn_percept('xxxx'))
  })