up <- ping_service("opsin")
test_that("opsin_query()", {
  skip_on_cran()
  skip_if_not(up, "OPSIN service is down")

  o1 <- opsin_query(c('Cyclopropane', 'Octane'))
  o2 <- suppressWarnings(opsin_query(c('xxxx')))

  # issue #146
  b1 <- opsin_query('Acetic acid')
  expect_equal(b1$query, 'Acetic acid')

  expect_s3_class(o1, 'data.frame')

  expect_equal(
    colnames(o1),
    c("query", "status", "message", "inchi", "stdinchi", "stdinchikey", "smiles")
  )

  expect_equal(nrow(o1), 2)
  expect_equal(nrow(o2), 1)
  expect_equal(o1$query, c('Cyclopropane', 'Octane'))
  expect_equal(o2$query, c('xxxx'))
  expect_equal(is.na(opsin_query(NA)$smiles), TRUE)
  expect_equal(ncol(o1), ncol(o2))
})
