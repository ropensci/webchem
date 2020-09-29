up <- ping_service("ci")
test_that("chemid returns correct results", {
  skip_on_cran()
  skip_if_not(up, "CHEMID service is down")

  o2 <- ci_query('50-00-0', from = 'rn')
  o3 <- ci_query('WSFSSNUMVMOOMR-UHFFFAOYSA-N', from = 'inchikey')
  expect_type(o2, 'list')
  expect_type(o3, 'list')

  o1 <- ci_query(c('xxxxx', NA, 'Aspirin', 'Triclosan'), from = 'name', match = 'best')
  expect_is(o1, 'list')

  expect_true(length(o1) == 4)
  expect_true(is.na(o1[[1]]))
  expect_true(is.na(o1[[2]]))

  expect_equal(o1[[3]]$name[2], "Aspirin")
  expect_length(o1[[3]], 9)
  expect_s3_class(o1[[3]]$physprop, "data.frame")

  b1 <- ci_query('Tetracyclin', from = 'name')
  expect_equal(b1[[1]]$name[1], "Tetracycline")
  b2 <- ci_query('Edetic acid', from = 'name', match = 'best')
  expect_equal(b2[[1]]$name[1], "Edetic acid")

  # test multiple matches
  m1 <- ci_query('Tetracyclin', from = 'name', match = 'first')
  m2 <- b1 #best is default
  m3 <- ci_query('Tetracyclin', from = 'name', match = 'na')

  expect_type(m1, 'list')
  expect_type(m2, 'list')
  expect_type(m3, 'list')
  expect_equal(m1[[1]]$cas, "60-54-8")
  expect_equal(m2[[1]]$cas, "60-54-8")
  expect_equal(m3[[1]], NA)
})
