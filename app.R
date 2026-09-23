library(shiny)

# Keep the entry point deliberately small: UI and server stay independently
# readable while this folder remains runnable with shiny::runApp("app.R").
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shinyApp(ui = ui, server = server)

