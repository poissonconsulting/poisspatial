context("blob")

test_that("blob works", {
  dir <- file.path(system.file(package = "poisspatial"))
  blob <- blob(dir, "[.]pdf$")
  expect_identical(colnames(blob), c("File", "Sub", "BLOB"))
  expect_identical(blob$File, c("seb-dalgarno.pdf", "joe-thorley.pdf"))
})
