context("alanwood")

test_that("alanwood, commonname", {
  fl <- alanwood('Fluazinam', type = 'commonname')
  xx <- alanwood('xxxxx', type = 'commonname')

  expect_equal(fl$cas, "79622-59-6", verbose = FALSE)
  expect_equal(xx, NA, verbose = FALSE)
  expect_equal(length(fl), 9)
})


test_that("alanwood, cas", {
  cs <-  alanwood("79622-59-6", type = 'cas')
  xx <- alanwood('xxxxx', type = 'cas')

  expect_equal(cs$cas, "79622-59-6", verbose = FALSE)
  expect_equal(cs$cname, "fluazinam", verbose = FALSE)
  expect_equal(xx, NA, verbose = FALSE)
})
