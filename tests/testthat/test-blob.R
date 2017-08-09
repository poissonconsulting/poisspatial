context("blob")

test_that("blob works", {
  dir <- file.path(system.file(package = "poisspatial"))
  blob <- blob(dir, "[.]pdf$")
  expect_identical(colnames(blob), c("FileName", "BLOB"))
  expect_identical(blob$FileName, c("joe-thorley", "seb-dalgarno"))
})
