library(testthat)

app_dir <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)

test_that("Connect Cloud deployment manifest is present and records the app dependencies", {
  manifest_path <- file.path(app_dir, "manifest.json")

  expect_true(file.exists(manifest_path))
  if (file.exists(manifest_path)) {
    manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
    expect_true(all(c("shiny", "ganttrify", "MetBrewer", "readxl") %in% names(manifest$packages)))
  }
})
