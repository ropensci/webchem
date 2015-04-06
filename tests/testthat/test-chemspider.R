context("chemspider")
token <- '37bf5e57-9091-42f5-9274-650a64398aaf'
tmp <- get_csid("Triclosan", token = token)

test_that("correct result", {
  expect_equal(tmp, '5363')
})