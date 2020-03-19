context('chebi')

a <- get_chebiid('Glyphosate', category = 'ALL')
b <- get_chebiid(c("triclosan","glyphosate","balloon",NA))
A <- chebi_comp_entity('CHEBI:27744')
B <- chebi_comp_entity('27732')

test_that('chebi returns correct results', {
  skip_on_cran()

  expect_is(a, 'list')
  expect_is(a[[1]], 'data.frame')
  expect_is(b, "list")
  expect_is(A, 'list')

  expect_equal(names(a[[1]])[1], 'chebiid')
  expect_equal(mean(names(b[[1]])==names(b[[2]])),1) # check structure
  expect_equal(mean(names(b[[2]])==names(b[[3]])),1) # check structure
  expect_equal(mean(names(b[[3]])==names(b[[4]])),1) # check structure
  expect_equal(A$`CHEBI:27744`$regnumbers$data[1], '1071-83-6')
  expect_equal(B$`27732`$properties$chebiasciiname, 'caffeine')
  expect_equal(B$`27732`$properties$entitystar, '3')
})
