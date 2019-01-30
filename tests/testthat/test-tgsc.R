context("goodscents")

test_that('tgsc_percept()', {
  skip_on_cran()

  a = tgsc_percept('122-78-1')
  b = tgsc_percept(c('122-78-1', '127-91-3'))
  c = tgsc_percept('122-78-1', odor = T)
  d = tgsc_percept(c('122-78-1', '127-91-3'), odor = T)

  expect_equivalent(a, 'green sweet floral hyacinth clover honey cocoa')
  expect_equivalent(b, c('green sweet floral hyacinth clover honey cocoa',
                    'dry woody resinous pine hay green'))
  expect_equal(a, c)
  expect_equal(b, d)

  e = tgsc_percept('122-78-1', odor = F, flavor = T)
  f = tgsc_percept(c('122-78-1', '127-91-3'), odor = F, flavor = T)

  expect_equivalent(e, 'honey, sweet, floral, chocolate and cocoa, with a spicy nuance')
  expect_equivalent(f, c('honey, sweet, floral, chocolate and cocoa, with a spicy nuance',
                    'fresh, piney and woody, terpy and resinous with a slight minty, camphoraceous with a spicy nuance'))

  g = tgsc_percept('122-78-1', flavor = T)
  h = tgsc_percept(c('122-78-1', '127-91-3'), flavor = T)

  expect_equivalent(g, list(c('green sweet floral hyacinth clover honey cocoa', 'honey, sweet, floral, chocolate and cocoa, with a spicy nuance')))
  expect_equivalent(h, list(c('green sweet floral hyacinth clover honey cocoa', 'honey, sweet, floral, chocolate and cocoa, with a spicy nuance'),
                    c('dry woody resinous pine hay green', 'fresh, piney and woody, terpy and resinous with a slight minty, camphoraceous with a spicy nuance')))
})
