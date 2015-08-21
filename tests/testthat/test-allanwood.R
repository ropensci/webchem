context("allanwood")

test_that("allanwood, commonname", {
  fl <- allanwood('Fluazinam', type = 'commonname')
  xx <- allanwood('xxxxx', type = 'commonname')

  expect_equal(fl$cas, "79622-59-6", verbose = FALSE)
  expect_equal(xx, NA, verbose = FALSE)
  expect_equal(length(fl), 9)
})


test_that("allanwood, cas", {
  cs <-  allanwood("79622-59-6", type = 'cas')
  xx <- allanwood('xxxxx', type = 'cas')

  expect_equal(cs$cas, "79622-59-6", verbose = FALSE)
  expect_equal(cs$cname, "fluazinam", verbose = FALSE)
  expect_equal(xx, NA, verbose = FALSE)
})
