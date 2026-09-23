library(testthat)

app_dir <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
source(file.path(app_dir, "server.R"), local = TRUE)

test_that("annotation data follows ganttrify spot format", {
  expect_identical(annotation_columns, c("activity", "spot_type", "spot_date"))
})

test_that("demo uses a biomedical undergraduate midterm and research timeline", {
  expect_identical(
    unique(demo_schedule$wp),
    c(
      "WP1 Midterm Exam Preparation",
      "WP2 Research Progress"
    )
  )
  expect_true("Midterm examination" %in% demo_schedule$activity)
  expect_identical(
    demo_schedule$end_date[demo_schedule$activity == "Research progress presentation"],
    as.Date("2026-11-05")
  )
  expect_true("Midterm examination" %in% demo_annotations$activity)
  expect_true("Research progress presentation" %in% demo_annotations$activity)
})

test_that("plot height scales with activity and work-package rows", {
  expect_equal(plot_height_for_schedule(demo_schedule), 600)
  expect_equal(plot_height_for_schedule(demo_schedule[1, ]), 600)
})

test_that("date previews use unambiguous year-month-day labels", {
  formatted <- format_preview_dates(data.frame(start_date = as.Date("2026-09-01"), end_date = as.Date("2026-09-30")))

  expect_identical(formatted$start_date, "2026-09-01")
  expect_identical(formatted$end_date, "2026-09-30")
})

test_that("invalid uploads do not render inline preview errors", {
  expect_null(safe_preview(function() stop("Invalid file structure")))
})

test_that("ganttrify receives the requested axis and visual settings", {
  source_text <- paste(readLines(file.path(app_dir, "server.R")), collapse = "\n")

  expect_match(source_text, 'month_number_label = TRUE')
  expect_match(source_text, 'month_date_label = TRUE')
  expect_match(source_text, 'axis_text_align = "left"')
  expect_match(source_text, 'size_activity = settings\\$activity_line_width')
  expect_match(source_text, 'size_wp = settings\\$wp_line_width')
  expect_match(source_text, 'alpha_wp = settings\\$wp_opacity')
  expect_match(source_text, 'alpha_activity = settings\\$activity_opacity')
  expect_match(source_text, 'size_text_relative = settings\\$text_size')
  expect_match(source_text, 'mark_years = settings\\$show_year_boundaries')
  expect_match(source_text, 'mark_quarters = FALSE')
  expect_match(source_text, 'colour_stripe = settings\\$month_stripe_colour')
  expect_match(source_text, 'line_end_wp = "square"')
  expect_match(source_text, 'line_end_activity = "round"')
  expect_match(source_text, 'spot_padding = grid::unit\\(settings\\$annotation_padding, "lines"\\)')
  expect_match(source_text, 'spot_size_text_relative = settings\\$annotation_text_size')
  expect_match(source_text, 'label_wrap = FALSE')
  expect_match(source_text, 'wp_label_bold = TRUE')
  expect_match(source_text, 'hjust = 0.5')
  expect_match(source_text, 'eventReactive\\(input\\$demo_update_plot')
  expect_match(source_text, 'eventReactive\\(input\\$user_update_plot')
  expect_match(source_text, 'demo_appearance <- reactive')
  expect_match(source_text, 'user_appearance <- reactive')
  expect_match(source_text, 'modifyList\\(demo_settings\\(\\), demo_appearance\\(\\)\\)')
  expect_match(source_text, 'modifyList\\(user_settings\\(\\), user_appearance\\(\\)\\)')
})

test_that("immediate plot appearance reactives do not use eventReactive-only arguments", {
  source_text <- paste(readLines(file.path(app_dir, "server.R")), collapse = "\n")

  expect_false(grepl("demo_appearance <- reactive\\(\\{[\\s\\S]*?\\}, ignoreNULL = FALSE\\)", source_text))
  expect_false(grepl("user_appearance <- reactive\\(\\{[\\s\\S]*?\\}, ignoreNULL = FALSE\\)", source_text))
})
