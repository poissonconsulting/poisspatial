context("dms")

test_that("dms", {
  expect_identical(ps_dd2ddm(c(70.654, NA, -56.654)),
                   c("70\u00B0 39.2399999999998'", NA, "-56\u00B0 39.2400000000002'"))
  expect_identical(ps_dd2ddm(c(NA_real_)),
                   c(NA_character_))

  expect_identical(ps_dm2mds(c(10.654, NA)),c("10' 39.24\"", NA))

  expect_identical(ps_dd2dmds(c(70.654, NA, -56.654)),
                   c("70\u00B0 39' 14.3999999999869\"", NA, "-56\u00B0 39' 14.4000000000125\u0022"))
})
