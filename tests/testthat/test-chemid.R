up <- ping_service("ci")
test_that("chemid returns correct results", {
  skip_on_cran()
  skip_if_not(up, "CHEMID service is down")

  o2 <- ci_query('50-00-0', from = 'rn')
  o3 <- ci_query('WSFSSNUMVMOOMR-UHFFFAOYSA-N', from = 'inchikey')
  expect_type(o2, 'list')
  expect_type(o3, 'list')
})
