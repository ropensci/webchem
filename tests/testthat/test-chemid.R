context("chemid")


test_that("chemid returns correct results", {
  xx <- ci_query('xxxxx', type = 'name')
  fl <- ci_query('50-00-0', type = 'rn')
  inc <- ci_query('WSFSSNUMVMOOMR-UHFFFAOYSA-N', type = 'inchikey')

  # test multiple matches
  m1 <- ci_query('Tetracyclin', type = 'name', mult = 'first')
  m2 <- ci_query('Tetracyclin', type = 'name', mult = 'best')
  m3 <- ci_query('Tetracyclin', type = 'name', mult = 'na')

  b1 <- ci_query('Tetracyclin', type = 'name')  # BUG: Failed because of multiple matches
  expect_equal(b1$name[1], "Tetracycline")

  expect_equal(fl$cas, "50-00-0")
  expect_equal(inc$cas, "50-00-0")
  expect_equal(fl$name[1], "Formaldehyde [USP]")
  expect_equal(fl$physprop$Value[fl$physprop[ , 1] == 'Water Solubility'], 400000)
  expect_equal(length(fl), 9)
  expect_true(is.data.frame(fl$physprop))
  expect_equal(xx, NA)

  expect_equal(m1$cas, "60-54-8")
  expect_equal(m2$cas, "60-54-8")
  expect_equal(m3, NA)
})



