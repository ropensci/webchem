require(RCurl)

qurl <- "http://opsin.ch.cam.ac.uk/opsin/"
cont <- try(getURL(qurl, .encoding = 'UTF-8', .opts = list(timeout = 3)),
            silent = TRUE)
down <- inherits(cont, 'try-error')

test_that("opsin_query()", {
  skip_on_cran()
  skip_if(down, "OPSIN service is down")

  o1 <- opsin_query(c('Cyclopropane', 'Octane'))
  o2 <- suppressWarnings(opsin_query(c('xxxx')))

  # issue #146
  b1 <- opsin_query('Acetic acid')
  expect_equal(b1$query, 'Acetic acid')

  expect_s3_class(o1, 'data.frame')

  expect_equal(
    colnames(o1),
    c("inchi", "stdinchi", "stdinchikey", "smiles", "message", "status", "query")
  )
  expect_equal(ncol(o1), ncol(o2))
  expect_equal(nrow(o1), 2)
  expect_equal(nrow(o2), 1)
  expect_equal(o1$query, c('Cyclopropane', 'Octane'))
  expect_equal(o2$query, c('xxxx'))
})


