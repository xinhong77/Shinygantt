library(testthat)

app_dir <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)

test_that("README links readers to the public Shinygantt demo", {
  readme_path <- file.path(app_dir, "README.md")
  readme <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")

  expect_match(readme, "## Live demo")
  expect_match(readme, "https://01a0cc62-dd4e-09eb-da14-0b1d9c01126e.share.connect.posit.cloud/")
})

test_that("Connect Cloud deployment manifest is present and records the app dependencies", {
  manifest_path <- file.path(app_dir, "manifest.json")

  expect_true(file.exists(manifest_path))
  if (file.exists(manifest_path)) {
    manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
    expect_true(all(c("shiny", "ganttrify", "MetBrewer", "readxl") %in% names(manifest$packages)))
    expect_identical(manifest$packages$ganttrify$Source, "github")
    expect_identical(manifest$packages$ganttrify$description$RemoteUsername, "giocomai")
    expect_identical(manifest$packages$ganttrify$description$RemoteRepo, "ganttrify")
  }
})
