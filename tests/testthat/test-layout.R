library(testthat)
library(htmltools)
library(shiny)

app_dir <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
source(file.path(app_dir, "ui.R"), local = TRUE)

test_that("the palette selector includes every MetBrewer palette", {
  expect_true(exists("palette_choices"))
  if (exists("palette_choices")) {
    expect_setequal(palette_choices, names(MetBrewer::MetPalettes))
  }
})

test_that("the interface uses an enlarged base font size", {
  css <- paste(readLines(file.path(app_dir, "www", "styles.css")), collapse = "\n")

  expect_match(css, "font-size:20px")
  expect_match(css, "selectize-dropdown")
  expect_match(css, "font-size:1rem")
})

test_that("the application uses the ShinySC2.0 Comic Sans MS font family", {
  source_text <- paste(readLines(file.path(app_dir, "ui.R")), collapse = "\n")
  css <- paste(readLines(file.path(app_dir, "www", "styles.css")), collapse = "\n")

  expect_match(source_text, 'base_font = "Comic Sans MS"')
  expect_match(source_text, 'heading_font = "Comic Sans MS"')
  expect_match(css, 'font-family:"Comic Sans MS"')
})

test_that("demo controls appear before the demo Gantt plot", {
  markup <- renderTags(ui)$html

  expect_match(markup, ">Demo</h2>")
  expect_match(markup, 'id="demo_update_plot"')
  expect_match(markup, 'id="demo_gantt_container"')
  expect_match(markup, 'class="row workspace-columns"')
  expect_match(markup, 'id="demo_activity_line_width"')
  expect_match(markup, 'id="demo_wp_line_width"')
  expect_match(markup, 'id="demo_wp_opacity"')
  expect_match(markup, 'id="demo_show_year_boundaries"')
  expect_match(markup, 'id="demo_month_stripe_colour"')
  expect_match(markup, 'id="demo_annotation_padding"')
  expect_match(markup, 'id="demo_annotation_text_size"')
  expect_match(markup, ">Major tasks</h5>")
  expect_match(markup, ">Annotation labels</h5>")
  expect_match(markup, 'id="demo_activity_opacity"')
  expect_match(markup, 'id="demo_text_size"')
  expect_gte(length(gregexpr('class="plot-options"', markup)[[1]]), 2)
  expect_lt(
    regexpr('id="demo_update_plot"', markup)[1],
    regexpr('id="demo_gantt_container"', markup)[1]
  )
})

test_that("plot options expose adjustable dimensions for both workspaces", {
  markup <- renderTags(ui)$html
  css <- paste(readLines(file.path(app_dir, "www", "styles.css")), collapse = "\n")

  expect_match(markup, 'id="demo_plot_height"')
  expect_match(markup, 'id="demo_plot_width"')
  expect_match(markup, 'id="user_plot_height"')
  expect_match(markup, 'id="user_plot_width"')
  expect_match(markup, 'id="user_wp_line_width"')
  expect_match(markup, 'id="user_wp_opacity"')
  expect_match(markup, 'id="user_show_year_boundaries"')
  expect_match(markup, 'id="user_month_stripe_colour"')
  expect_match(markup, 'id="user_annotation_padding"')
  expect_match(markup, 'id="user_annotation_text_size"')
  expect_match(markup, 'id="demo_gantt_container"')
  expect_match(markup, 'id="user_gantt_container"')
  expect_match(css, "overflow:auto")
})

test_that("chart updates and file uploads expose a short running overlay", {
  rendered <- renderTags(ui)
  markup <- rendered$html
  head_markup <- rendered$head
  css <- paste(readLines(file.path(app_dir, "www", "styles.css")), collapse = "\n")

  expect_match(markup, 'id="gantt-busy-overlay"')
  expect_match(markup, "Updating chart")
  expect_match(head_markup, "gantt-overlay")
  expect_match(head_markup, "#main_schedule_file, #annotation_file")
  expect_match(css, "\\.gantt-busy-overlay")
  expect_match(markup, "fa-spin")
})

test_that("workspace panels are collapsible and demo format starts collapsed", {
  markup <- renderTags(ui)$html

  expect_match(markup, 'data-bs-target="#demo-section"')
  expect_match(markup, 'data-bs-target="#upload-section"')
  expect_match(markup, 'data-bs-target="#workspace-section"')
  expect_match(markup, 'id="demo-template-details"')
  expect_match(markup, 'id="download_demo_template"')
  expect_match(markup, "Main Excel requirements")
  expect_match(markup, "Annotation Excel")
  expect_match(markup, 'class="template-pair"')
})

test_that("the two uploaded file previews are displayed side by side", {
  markup <- renderTags(ui)$html

  expect_match(markup, 'class="preview-pair"')
  expect_gte(length(gregexpr('class="preview-table"', markup)[[1]]), 2)
})

test_that("collapsible headers use a dashboard-style card treatment", {
  css <- paste(readLines(file.path(app_dir, "www", "styles.css")), collapse = "\n")

  expect_match(css, "workspace-box__header")
  expect_match(css, "collapse-toggle")
  expect_match(css, "linear-gradient")
})

test_that("plot-options menus remain usable when their controls exceed the card height", {
  css <- paste(readLines(file.path(app_dir, "www", "styles.css")), collapse = "\n")

  expect_match(css, ".workspace-box {", fixed = TRUE)
  expect_match(css, "overflow:visible", fixed = TRUE)
  expect_match(css, ".plot-options .dropdown-menu { max-height", fixed = TRUE)
  expect_match(css, "overflow-y:auto", fixed = TRUE)
})

test_that("uploaded-data controls appear before the user Gantt plot", {
  markup <- renderTags(ui)$html

  expect_match(markup, 'id="main_schedule_file"')
  expect_match(markup, 'id="annotation_file"')
  expect_match(markup, 'class="upload-pair"')
  expect_match(markup, ">Data preview</h3>")
  expect_false(grepl("Main file: wp", markup, fixed = TRUE))
  expect_false(grepl(">Main schedule</h4>", markup, fixed = TRUE))
  expect_false(grepl(">Annotation spots</h4>", markup, fixed = TRUE))
  expect_gte(length(gregexpr('class="row workspace-columns"', markup)[[1]]), 3)
  expect_match(markup, 'id="user_update_plot"')
  expect_match(markup, 'id="user_gantt_container"')
  expect_lt(
    regexpr('id="user_update_plot"', markup)[1],
    regexpr('id="user_gantt_container"', markup)[1]
  )
})
