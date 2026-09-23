# Shinygantt

Shinygantt is a small example of how an R-based visualization workflow can be transformed from code into an interactive graphical user interface. It creates clear Gantt charts from Excel schedules without requiring users to write R code.

## Live demo

Try the deployed application here: [Open Shinygantt](https://01a0cc62-dd4e-09eb-da14-0b1d9c01126e.share.connect.posit.cloud/).

![From R code to an interactive biomedical research visualization](www/images/r-to-shinygantt-workflow.png)

## From R code to a graphical interface

R is a programming language and computing environment widely used for statistical analysis, data science, and data visualization. Packages such as `ggplot2` help researchers create scatter plots, bar charts, box plots, heatmaps, volcano plots, PCA plots, and time-series plots.

Some R packages standardize a specific visualization task on top of this foundation. In this project, `ganttrify` organizes work packages, activities, dates, and milestone annotations into a consistent Gantt-chart workflow. `MetBrewer` provides curated colour palettes for the chart.

Shinygantt wraps these R tools in a Shiny web application. Users can upload Excel files, preview and validate the schedule, adjust chart settings, and generate a customized timeline in the browser. This demonstrates a simple automated visualization pipeline:

```text
Excel schedule -> data validation -> standardized Gantt chart -> interactive Shiny interface
```

## Why this matters for biomedical research

Biomedical research frequently involves complex genomic, transcriptomic, proteomic, and metabolomic data. These datasets often require a similar path from raw data, through processing and visualization, to an interface that researchers can use repeatedly. Turning reliable R workflows into graphical tools can lower the coding barrier and make data exploration and communication more accessible.

## Run the app

From R, run:

```r
shiny::runApp()
```

## Excel formats

The app accepts two Excel files.

### Main schedule

| Column | Description |
| --- | --- |
| `wp` | Major task or work package name. |
| `activity` | Individual task name. |
| `start_date` | Task start date in `YYYY-MM-DD` format. |
| `end_date` | Task end date in `YYYY-MM-DD` format. |

### Annotation file (optional)

| Column | Description |
| --- | --- |
| `activity` | Must exactly match an activity in the main schedule. |
| `spot_type` | Short milestone label, such as `Exam` or `Presentation`. |
| `spot_date` | Milestone date in `YYYY-MM-DD` format. |

Example files are available in [`Excel/`](Excel/).

## Main R packages

- `shiny` and `bslib` for the interactive web interface.
- `ganttrify` for standardized Gantt-chart generation.
- `ggplot2` for the underlying plotting system.
- `MetBrewer` for colour palettes.
- `readxl` for reading uploaded Excel files.
