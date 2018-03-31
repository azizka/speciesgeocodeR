library(speciesgeocodeR)
context("SpGeoCod")

set.seed(1)

# Coordinate level cleaning
data(lemurs)
data(mdg_biomes)

outp <- 

test_that("cc_cap arguments work", {
  expect_equal(length(SpGeoCod(lemurs, mdg_biomes, areanames = "name")), 5)
})