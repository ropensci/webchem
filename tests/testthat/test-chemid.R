context("chemid")


test_that("chemid returns correct results", {
  o1 <- ci_query(c('xxxxx', NA, 'Aspirin', 'Triclosan'), type = 'name', match = 'best')
  o2 <- ci_query('50-00-0', type = 'rn')
  o3 <- ci_query('WSFSSNUMVMOOMR-UHFFFAOYSA-N', type = 'inchikey')

  # test multiple matches
  m1 <- ci_query('Tetracyclin', type = 'name', match = 'first')
  m2 <- ci_query('Tetracyclin', type = 'name', match = 'best')
  m3 <- ci_query('Tetracyclin', type = 'name', match = 'na')

  b1 <- ci_query('Tetracyclin', type = 'name')  # BUG: Failed because of multiple matches
  expect_equal(b1[[1]]$name[1], "Tetracycline")

  expect_is(o1, 'list')
  expect_is(o2, 'list')
  expect_is(o3, 'list')
  expect_is(m1, 'list')
  expect_is(m2, 'list')
  expect_is(m31, 'list')

  expect_true(length(o1) == 4)
  expect_true(is.na(o1[[1]]))
  expect_true(is.na(o1[[2]]))


  expect_equal(o1[[3]]$name[2], "Aspirin")
  expect_equal(length(o1[[3]]), 9)
  expect_true(is.data.frame(o1[[3]]$physprop))

  expect_equal(m1[[1]]$cas, "60-54-8")
  expect_equal(m2[[1]]$cas, "60-54-8")
  expect_equal(m3[[1]], NA)
})



