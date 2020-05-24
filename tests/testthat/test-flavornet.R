up <- ping_service("fn")
test_that("fn_percept()", {
  skip_on_cran()
  skip_if_not(up, "Flavornet is unreachable")

  a <- fn_percept("123-32-0")
  b <- fn_percept(c("75-07-0", "123-32-0"))
  c <- suppressWarnings(fn_percept(c("75-07-0", "123-32-0", "50-00-0")))

  expect_type(a, 'character')
  expect_type(b, 'character')
  expect_type(c, 'character')
  expect_equal(length(a), 1)
  expect_equal(length(b), 2)
  expect_equal(length(c), 3)

  expect_equal(a, structure("cocoa, roasted nut, roast beef, medicine",
                            .Names = "123-32-0"))
  expect_equal(b, structure(c("pungent, ether", "cocoa, roasted nut, roast beef, medicine"
  ), .Names = c("75-07-0", "123-32-0")))
  expect_true(is.na(c[[3]]))

  expect_warning(fn_percept('xxxx'))
})
