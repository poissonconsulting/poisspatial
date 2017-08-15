context("blob")

test_that("blob works", {
  dir <- file.path(system.file(package = "poisspatial"))
  blob <- ps_blob(dir)
  expect_identical(colnames(blob), c("FileName", "SubDirectory", "BLOB"))
  expect_identical(blob$FileName, c("seb-dalgarno.pdf", "joe-thorley.pdf"))
  expect_identical(blob$SubDirectory, c(".", "sub"))
})
