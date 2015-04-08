context("pan")

test_that("pan()", {
  expect_error(pan(c('xxxxx', 'aaaaaaa')))
  expect_equal(pan('xxxxx', verbose = FALSE), NA)
  expect_equal(length(pan('2,4-dichlorophenol', verbose = FALSE)), 73)
  expect_equal(pan('Triclosan', verbose = FALSE)$`CAS Number`, "3380-34-5")
  expect_equal(length(pan('Chlorpyrifos', verbose = FALSE, first = TRUE)[[1]]), 1)
})