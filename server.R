library(ggplot2)
library(dplyr)
library(MetBrewer)

main_columns <- c("wp", "activity", "start_date", "end_date")
annotation_columns <- c("activity", "spot_type", "spot_date")

# Demo content mirrors the biomedical undergraduate Excel templates.
demo_schedule <- data.frame(
  wp = c(
    "WP1 Midterm Exam Preparation", "WP1 Midterm Exam Preparation", "WP1 Midterm Exam Preparation",
    "WP2 Research Progress", "WP2 Research Progress", "WP2 Research Progress", "WP2 Research Progress"
  ),
  activity = c(
    "Review lecture notes", "Practice exam questions", "Midterm examination",
    "Read background literature", "Analyse pilot experiment data", "Write research progress report", "Research progress presentation"
  ),
  start_date = as.Date(c(
    "2026-09-15", "2026-10-01", "2026-10-16",
    "2026-09-15", "2026-10-06", "2026-10-20", "2026-11-05"
  )),
  end_date = as.Date(c(
    "2026-10-05", "2026-10-15", "2026-10-16",
    "2026-10-10", "2026-10-24", "2026-10-31", "2026-11-05"
  )),
  stringsAsFactors = FALSE
)

demo_annotations <- data.frame(
  activity = c(
    "Midterm examination", "Write research progress report", "Research progress presentation"
  ),
  spot_type = c("Exam", "Report due", "Presentation"),
  spot_date = as.Date(c("2026-10-16", "2026-10-31", "2026-11-05")),
  stringsAsFactors = FALSE
)

read_uploaded_table <- function(file_info) {
  extension <- tolower(tools::file_ext(file_info$name))
  table <- if (extension == "csv") {
    utils::read.csv(file_info$datapath, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    as.data.frame(readxl::read_excel(file_info$datapath))
  }
  names(table) <- tolower(trimws(names(table)))
  table
}

validate_main_schedule <- function(data) {
  missing_columns <- setdiff(main_columns, names(data))
  if (length(missing_columns)) stop(sprintf("Main file is missing: %s", paste(missing_columns, collapse = ", ")))
  data |>
    transmute(wp = trimws(as.character(wp)), activity = trimws(as.character(activity)), start_date = as.Date(start_date), end_date = as.Date(end_date)) |>
    filter(nzchar(wp), nzchar(activity), !is.na(start_date), !is.na(end_date), end_date >= start_date)
}

validate_annotations <- function(data) {
  missing_columns <- setdiff(annotation_columns, names(data))
  if (length(missing_columns)) stop(sprintf("Annotation file is missing: %s", paste(missing_columns, collapse = ", ")))
  data |>
    transmute(
      activity = trimws(as.character(activity)),
      spot_type = trimws(as.character(spot_type)),
      spot_date = as.Date(spot_date)
    ) |>
    filter(nzchar(activity), nzchar(spot_type), !is.na(spot_date))
}

format_preview_dates <- function(data) {
  date_columns <- intersect(c("start_date", "end_date", "spot_date"), names(data))
  for (column_name in date_columns) {
    data[[column_name]] <- format(as.Date(data[[column_name]]), "%Y-%m-%d")
  }
  data
}

safe_preview <- function(reader) {
  tryCatch(format_preview_dates(reader()), error = function(error) NULL)
}

safe_read <- function(reader) {
  tryCatch(reader(), error = function(error) NULL)
}

build_gantt_chart <- function(schedule, annotations, settings) {
  palette <- MetBrewer::met.brewer(settings$palette, n = max(3, length(unique(schedule$wp))), type = "discrete")
  spots <- if (settings$show_annotations) {
    annotations |>
      transmute(activity, spot_type, spot_date = format(spot_date, "%Y-%m-%d"))
  } else {
    NULL
  }

  ganttrify::ganttrify(
    project = schedule |>
      transmute(wp, activity, start_date = format(start_date, "%Y-%m-%d"), end_date = format(end_date, "%Y-%m-%d")),
    spots = spots,
    by_date = TRUE,
    exact_date = TRUE,
    project_start_date = min(schedule$start_date),
    colour_palette = palette,
    font_family = "Comic Neue",
    mark_quarters = FALSE,
    mark_years = settings$show_year_boundaries,
    colour_stripe = settings$month_stripe_colour,
    size_wp = settings$wp_line_width,
    wp_label_bold = TRUE,
    size_activity = settings$activity_line_width,
    alpha_wp = settings$wp_opacity,
    alpha_activity = settings$activity_opacity,
    line_end_wp = "square",
    line_end_activity = "round",
    label_wrap = FALSE,
    month_number_label = TRUE,
    month_date_label = TRUE,
    spot_padding = grid::unit(settings$annotation_padding, "lines"),
    spot_fill = ggplot2::alpha("white", 0.94),
    spot_border = 0,
    size_text_relative = settings$text_size,
    spot_size_text_relative = settings$annotation_text_size,
    axis_text_align = "left"
  ) +
    labs(title = settings$title) +
    theme(
      plot.title = element_text(face = "bold", colour = "#133b5c", size = 17, hjust = 0.5, margin = margin(b = 14)),
      axis.text.y = element_text(lineheight = 1.2),
      panel.grid.major.y = element_line(colour = "#D7E1EA", linewidth = 0.35),
      panel.grid.minor.y = element_blank()
    )
}

plot_height_for_schedule <- function(schedule, minimum = 600L, row_height = 42L, header_height = 150L) {
  visual_rows <- nrow(schedule) + length(unique(schedule$wp))
  max(minimum, header_height + row_height * visual_rows)
}

plot_dimension <- function(value, default) {
  if (is.null(value) || !is.finite(value)) default else as.integer(value)
}

set_gantt_overlay <- function(session, action = c("show", "hide"), title = NULL, body = NULL, step = NULL) {
  session$sendCustomMessage(
    "gantt-overlay",
    list(action = match.arg(action), title = title, body = body, step = step)
  )
}

hide_gantt_overlay_after_flush <- function(session) {
  session$onFlushed(function() set_gantt_overlay(session, "hide"), once = TRUE)
}

server <- function(input, output, session) {
  main_schedule <- reactive({
    req(input$main_schedule_file)
    validate_main_schedule(read_uploaded_table(input$main_schedule_file))
  })
  annotations <- reactive({
    req(input$annotation_file)
    validate_annotations(read_uploaded_table(input$annotation_file))
  })
  observeEvent(input$main_schedule_file, {
    set_gantt_overlay(
      session, "show", "Checking main schedule",
      "Validating task names and dates in your uploaded workbook.",
      "Step 2 of 3 · Validating schedule"
    )
    tryCatch({
      recommended_height <- plot_height_for_schedule(main_schedule())
      updateSliderInput(
        session,
        "user_plot_height",
        max = max(2400L, recommended_height),
        value = recommended_height
      )
      set_gantt_overlay(
        session, "show", "Main schedule ready",
        "Your data preview and chart workspace are ready to use.",
        "Step 3 of 3 · Preparing preview"
      )
    }, error = function(error) {
      showNotification(paste("Could not read the main schedule:", error$message), type = "error", duration = 8)
    })
    hide_gantt_overlay_after_flush(session)
  }, ignoreInit = TRUE)
  observeEvent(input$annotation_file, {
    set_gantt_overlay(
      session, "show", "Checking annotation file",
      "Validating activity names, annotation types, and dates.",
      "Step 2 of 3 · Validating annotations"
    )
    tryCatch({
      annotations()
      set_gantt_overlay(
        session, "show", "Annotations ready",
        "Your annotation preview is ready to use.",
        "Step 3 of 3 · Preparing preview"
      )
    }, error = function(error) {
      showNotification(paste("Could not read the annotation file:", error$message), type = "error", duration = 8)
    })
    hide_gantt_overlay_after_flush(session)
  }, ignoreInit = TRUE)
  observeEvent(input$demo_update_plot, {
    set_gantt_overlay(
      session, "show", "Updating demo chart",
      "Applying your controls and rendering the timeline.",
      "Rendering Gantt chart"
    )
    hide_gantt_overlay_after_flush(session)
  }, ignoreInit = TRUE)
  observeEvent(input$user_update_plot, {
    set_gantt_overlay(
      session, "show", "Updating your chart",
      "Applying your controls and rendering the timeline.",
      "Rendering Gantt chart"
    )
    hide_gantt_overlay_after_flush(session)
  }, ignoreInit = TRUE)
  demo_settings <- eventReactive(input$demo_update_plot, {
    list(
      title = input$demo_title,
      palette = input$demo_palette,
      show_annotations = isTRUE(input$demo_show_annotations)
    )
  }, ignoreNULL = FALSE)
  demo_appearance <- reactive({
    list(
      wp_line_width = input$demo_wp_line_width,
      wp_opacity = input$demo_wp_opacity,
      activity_line_width = input$demo_activity_line_width,
      activity_opacity = input$demo_activity_opacity,
      text_size = input$demo_text_size,
      show_year_boundaries = isTRUE(input$demo_show_year_boundaries),
      month_stripe_colour = input$demo_month_stripe_colour,
      annotation_padding = input$demo_annotation_padding,
      annotation_text_size = input$demo_annotation_text_size
    )
  })
  user_settings <- eventReactive(input$user_update_plot, {
    list(
      title = input$user_title,
      palette = input$user_palette,
      show_annotations = isTRUE(input$user_show_annotations)
    )
  }, ignoreNULL = FALSE)
  user_appearance <- reactive({
    list(
      wp_line_width = input$user_wp_line_width,
      wp_opacity = input$user_wp_opacity,
      activity_line_width = input$user_activity_line_width,
      activity_opacity = input$user_activity_opacity,
      text_size = input$user_text_size,
      show_year_boundaries = isTRUE(input$user_show_year_boundaries),
      month_stripe_colour = input$user_month_stripe_colour,
      annotation_padding = input$user_annotation_padding,
      annotation_text_size = input$user_annotation_text_size
    )
  })

  output$demo_gantt_container <- renderUI({
    plotOutput(
      "demo_gantt_plot",
      height = paste0(plot_dimension(input$demo_plot_height, plot_height_for_schedule(demo_schedule)), "px"),
      width = paste0(plot_dimension(input$demo_plot_width, 1100), "px")
    )
  })
  output$user_gantt_container <- renderUI({
    plotOutput(
      "user_gantt_plot",
      height = paste0(plot_dimension(input$user_plot_height, 600), "px"),
      width = paste0(plot_dimension(input$user_plot_width, 1100), "px")
    )
  })
  output$demo_gantt_plot <- renderPlot(
    build_gantt_chart(demo_schedule, demo_annotations, modifyList(demo_settings(), demo_appearance())),
    res = 110,
    height = function() plot_dimension(input$demo_plot_height, plot_height_for_schedule(demo_schedule)),
    width = function() plot_dimension(input$demo_plot_width, 1100)
  )
  output$main_data_preview <- renderTable({
    if (is.null(input$main_schedule_file)) return(NULL)
    safe_preview(main_schedule)
  }, striped = TRUE, hover = TRUE, spacing = "s")
  output$annotation_data_preview <- renderTable({
    if (is.null(input$annotation_file)) return(NULL)
    safe_preview(annotations)
  }, striped = TRUE, hover = TRUE, spacing = "s")
  output$demo_main_template <- renderTable(
    format_preview_dates(demo_schedule),
    striped = TRUE, hover = TRUE, spacing = "s"
  )
  output$demo_annotation_template <- renderTable(
    format_preview_dates(demo_annotations),
    striped = TRUE, hover = TRUE, spacing = "s"
  )
  output$download_demo_template <- downloadHandler(
    filename = function() "shinygantt-demo-template.xlsx",
    content = function(file) {
      file.copy("Excel/Main_file.xlsx", file, overwrite = TRUE)
    }
  )
  output$user_gantt_plot <- renderPlot(
    {
      if (is.null(input$main_schedule_file)) return(NULL)
      schedule <- safe_read(main_schedule)
      if (is.null(schedule)) return(NULL)
      annotation_data <- if (is.null(input$annotation_file)) {
        data.frame(activity = character(), spot_type = character(), spot_date = as.Date(character()))
      } else {
        safe_read(annotations)
      }
      if (is.null(annotation_data)) annotation_data <- data.frame(activity = character(), spot_type = character(), spot_date = as.Date(character()))
      build_gantt_chart(schedule, annotation_data, modifyList(user_settings(), user_appearance()))
    },
    res = 110,
    height = function() plot_dimension(input$user_plot_height, 600),
    width = function() plot_dimension(input$user_plot_width, 1100)
  )
}
