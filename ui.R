library(bslib)

palette_choices <- names(MetBrewer::MetPalettes)

chart_controls <- function(prefix, default_title) {
  tagList(
    tags$p(class = "section-label", "CHART CONTROLS"),
    textInput(paste0(prefix, "_title"), "Title", value = default_title),
    selectInput(paste0(prefix, "_palette"), "Colour palette", choices = palette_choices, selected = "Hokusai1"),
    checkboxInput(paste0(prefix, "_show_annotations"), "Show task annotations", value = TRUE),
    actionButton(paste0(prefix, "_update_plot"), "Update plot", class = "primary-btn")
  )
}

plot_options <- function(prefix, default_height = 600) {
  div(
    class = "plot-options",
    shinyWidgets::dropdownButton(
      tags$h4("Plot options"),
      tags$h5(class = "plot-options__group", "Major tasks"),
      sliderInput(paste0(prefix, "_wp_line_width"), "Major-task bar thickness", min = 1, max = 8, value = 3.2, step = 0.2),
      sliderInput(paste0(prefix, "_wp_opacity"), "Major-task bar transparency", min = 0.2, max = 1, value = 0.9, step = 0.05),
      tags$h5(class = "plot-options__group", "Activities"),
      sliderInput(paste0(prefix, "_activity_line_width"), "Activity bar thickness", min = 1, max = 6, value = 2.2, step = 0.2),
      sliderInput(paste0(prefix, "_activity_opacity"), "Activity bar transparency", min = 0.2, max = 1, value = 0.85, step = 0.05),
      sliderInput(paste0(prefix, "_text_size"), "All chart text size", min = 0.7, max = 2.0, value = 1.3, step = 0.1),
      tags$h5(class = "plot-options__group", "Timeline"),
      checkboxInput(paste0(prefix, "_show_year_boundaries"), "Show year boundaries", value = FALSE),
      colourpicker::colourInput(paste0(prefix, "_month_stripe_colour"), "Alternate-month background colour", value = "#F1F5F9", showColour = "background"),
      sliderInput(paste0(prefix, "_plot_height"), "Chart height", min = 360, max = 2400, value = default_height, step = 20, post = " px"),
      sliderInput(paste0(prefix, "_plot_width"), "Chart width", min = 700, max = 1800, value = 1100, step = 50, post = " px"),
      tags$h5(class = "plot-options__group", "Annotation labels"),
      sliderInput(paste0(prefix, "_annotation_padding"), "Annotation label padding", min = 0, max = 0.8, value = 0.12, step = 0.04),
      sliderInput(paste0(prefix, "_annotation_text_size"), "Annotation label text size", min = 0.5, max = 2, value = 0.8, step = 0.1),
      circle = FALSE,
      status = "danger",
      icon = icon("cog"),
      width = "330px",
      size = "sm",
      tooltip = shinyWidgets::tooltipOptions(title = "Adjust plot appearance")
    )
  )
}

collapsible_workspace <- function(id, title, content, open = TRUE) {
  div(
    class = "workspace-box",
    div(
      class = "workspace-box__header",
      tags$h2(title),
      tags$button(
        type = "button",
        class = paste("collapse-toggle", if (!open) "collapsed"),
        `data-bs-toggle` = "collapse",
        `data-bs-target` = paste0("#", id),
        `aria-expanded` = tolower(as.character(open)),
        `aria-controls` = id,
        icon("chevron-down"),
        tags$span(class = "visually-hidden", paste("Toggle", title))
      )
    ),
    div(id = id, class = paste("collapse", if (open) "show"), div(class = "workspace-box__body", content))
  )
}

demo_template_panel <- function() {
  tags$details(
    id = "demo-template-details", class = "demo-template-details",
    tags$summary(icon("table"), "View Excel format template"),
    div(
      class = "demo-template-details__content",
      div(
        class = "template-pair",
        div(
          class = "template-column",
          tags$h3("Main Excel requirements"),
          tags$p("Use one row per task. These four columns are required:"),
          tags$ul(
            tags$li(tags$code("wp"), ": work-package or major task name."),
            tags$li(tags$code("activity"), ": individual task name."),
            tags$li(tags$code("start_date"), " and ", tags$code("end_date"), ": dates in YYYY-MM-DD format.")
          ),
          div(class = "preview-table", tableOutput("demo_main_template"))
        ),
        div(
          class = "template-column",
          tags$h3("Annotation Excel (optional)"),
          tags$p("Use this sheet to add milestone labels to an existing activity:"),
          tags$ul(
            tags$li(tags$code("activity"), ": must exactly match an activity in Main Excel."),
            tags$li(tags$code("spot_type"), ": short milestone label, such as Exam or Submission."),
            tags$li(tags$code("spot_date"), ": milestone date in YYYY-MM-DD format.")
          ),
          div(class = "preview-table", tableOutput("demo_annotation_template"))
        )
      ),
      div(class = "template-download-row", downloadButton("download_demo_template", "Download Excel template", icon = icon("download"), class = "template-download"))
    )
  )
}

ui <- page_fluid(
  theme = bs_theme(version = 5, bg = "#f4f8fc", fg = "#233444", primary = "#1f6aa5", base_font = "Comic Sans MS", heading_font = "Comic Sans MS"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML("(function() {
      var ganttOverlayHideTimer = null;
      function setGanttOverlay(message) {
        var overlay = document.getElementById('gantt-busy-overlay');
        if (!overlay) return;
        var title = document.getElementById('gantt-busy-title');
        var body = document.getElementById('gantt-busy-body');
        var step = document.getElementById('gantt-busy-step');
        if (message.title && title) title.textContent = message.title;
        if (message.body && body) body.textContent = message.body;
        if (message.step && step) step.textContent = message.step;
        if (message.action === 'hide') {
          ganttOverlayHideTimer = window.setTimeout(function() {
            overlay.style.display = 'none';
          }, 450);
        } else {
          if (ganttOverlayHideTimer) window.clearTimeout(ganttOverlayHideTimer);
          overlay.style.display = 'flex';
        }
      }

      Shiny.addCustomMessageHandler('gantt-overlay', setGanttOverlay);

      $(document).on('change', '#main_schedule_file, #annotation_file', function() {
        if (!this.files || !this.files.length) return;
        setGanttOverlay({
          action: 'show',
          title: 'Uploading Excel file',
          body: 'Reading the selected workbook and checking its Gantt columns.',
          step: 'Step 1 of 3 · Uploading file'
        });
      });

      $(document).on('click', '#demo_update_plot, #user_update_plot', function() {
        setGanttOverlay({
          action: 'show',
          title: this.id === 'demo_update_plot' ? 'Updating demo chart' : 'Updating your chart',
          body: 'Applying your chart controls and rendering the timeline.',
          step: 'Rendering Gantt chart'
        });
      });
    })();"))
  ),
  div(
    class = "app-shell",
    div(class = "app-header", tags$h1("Shinygantt"), tags$p("Create clear Gantt charts from uploaded Excel schedules.")),
    fluidRow(
      collapsible_workspace(
        "demo-section", "Demo",
        div(
          div(
            class = "row workspace-columns",
            column(3, div(class = "control-panel", chart_controls("demo", "Project Timeline"))),
            column(9, div(class = "plot-panel", plot_options("demo", default_height = 990), div(class = "plot-canvas", uiOutput("demo_gantt_container"))))
          ),
          demo_template_panel()
        )
      )
    ),
    fluidRow(
      collapsible_workspace(
        "upload-section", "Upload your Excel files",
        div(
          class = "row workspace-columns",
          column(
            6,
            div(
              class = "upload-panel",
              div(
                class = "upload-pair",
                div(class = "upload-zone", icon("file-excel"), tags$h3("Main Gantt Excel"), tags$p("Drag and drop here, or click to browse"), fileInput("main_schedule_file", NULL, accept = c(".xlsx", ".xls", ".csv"), buttonLabel = "Choose file", placeholder = "Excel or CSV")),
                div(class = "upload-zone", icon("file-excel"), tags$h3("Annotation Excel"), tags$p("Drag and drop here, or click to browse"), fileInput("annotation_file", NULL, accept = c(".xlsx", ".xls", ".csv"), buttonLabel = "Choose file", placeholder = "Excel or CSV"))
              )
            ),
          ),
          column(
            6,
            div(
              class = "preview-panel", tags$h3("Data preview"),
              div(
                class = "preview-pair",
                div(class = "preview-table", tableOutput("main_data_preview")),
                div(class = "preview-table", tableOutput("annotation_data_preview"))
              )
            )
          )
        )
      )
    ),
    fluidRow(
      collapsible_workspace(
        "workspace-section", "Your chart workspace",
        div(
          class = "row workspace-columns",
          column(3, div(class = "control-panel", chart_controls("user", "Your uploaded timeline"))),
          column(9, div(class = "plot-panel", plot_options("user"), div(class = "plot-canvas", uiOutput("user_gantt_container"))))
        )
      )
    ),
    div(
      id = "gantt-busy-overlay", class = "gantt-busy-overlay", role = "status", `aria-live` = "polite",
      div(
        class = "gantt-busy-overlay__card",
        icon("spinner", class = "fa-spin"),
        tags$strong(id = "gantt-busy-title", "Updating chart"),
        tags$p(id = "gantt-busy-body", "Applying your settings."),
        div(id = "gantt-busy-step", class = "gantt-busy-overlay__step", "Preparing chart")
      )
    )
  )
)
