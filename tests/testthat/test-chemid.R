up <- ping_service("ci")
test_that("chemid returns correct results", {
  skip_on_cran()
  skip_if_not(up, "CHEMID service is down")
  skip("failing tests below")

  o2 <- ci_query('50-00-0', type = 'rn')
  o3 <- ci_query('WSFSSNUMVMOOMR-UHFFFAOYSA-N', type = 'inchikey')
  expect_type(o2, 'list')
  expect_type(o3, 'list')

  o1 <- ci_query(c('xxxxx', NA, 'Aspirin', 'Triclosan'), type = 'name', match = 'best')
  expect_is(o1, 'list')

  expect_true(length(o1) == 4)
  expect_true(is.na(o1[[1]]))
  expect_true(is.na(o1[[2]]))

  expect_equal(o1[[3]]$name[2], "Aspirin")
  expect_length(o1[[3]], 9)
  expect_s3_class(o1[[3]]$physprop, "data.frame")

  b1 <- ci_query('Tetracyclin', type = 'name')
  expect_equal(b1[[1]]$name[1], "Tetracycline")
  b2 <- ci_query('Edetic acid', type = 'name', match = 'best')
  expect_equal(b2[[1]]$name[1], "Edetic acid")
  expect_equal(attr(b2[[1]],'distance'), 0)

  # test multiple matches
  m1 <- ci_query('Tetracyclin', type = 'name', match = 'first')
  m2 <- ci_query('Tetracyclin', type = 'name', match = 'best')
  m3 <- ci_query('Tetracyclin', type = 'name', match = 'na')

  expect_type(m1, 'list')
  expect_type(m2, 'list')
  expect_type(m3, 'list')
  expect_equal(m1[[1]]$cas, "60-54-8")
  expect_equal(m2[[1]]$cas, "60-54-8")
  expect_equal(m3[[1]], NA)
})
