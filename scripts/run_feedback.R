library(agricolae)
library(DT)
library(SensoMineR)
library(tidyverse)
library(ggradar)
library(shiny)
library(glue)
library(grDevices)
library(patchwork)
library(purrr)
library(readr)
library(stats)
library(tidyr)
library(gridExtra)


run_feedback<- function(answers_dir = "Answers", dest_url = NULL, panel_url = NULL,
                      numeric_range = c(0, 10), refresh_seconds = 15) {
  # Set default host/port, if not provided as `dest_url`.
  host <- getOption("shiny.host", "127.0.0.1")
  port <- getOption("shiny.port")
  if (!is.null(dest_url)) {
    dest_url <- strsplit(dest_url, ":")[[1]]
    if (length(dest_url) != 2) {
      stop("`dest_url` should follow the format HOST:PORT , for example, 192.168.100.7:4001 .")
    }
    host <- dest_url[[1]]
    port <- as.numeric(dest_url[[2]])
  }

  files <- dir(answers_dir, full.names = TRUE)
  # Only keep directories (remove `diseno.csv`).
  files <- files[dir.exists(files)]

  files <- dir(files, full.names = TRUE, recursive = TRUE)
  answers <- data.frame(
    Producto = sub("\\.csv$", "", basename(files)),
    Valuador = basename(dirname(files))
  ) %>%
    bind_cols(read_csv(files, col_types = cols()))
  req(nrow(answers) > 0)
  arrange(answers, Producto, Valuador)%>%
    separate(Producto, c("Producto", "Rep"))



  ui <- fluidPage(

      mainPanel(
        tabsetPanel(
          tabPanel(
            "Calibración",

            br(),
            selectInput("panelista", "Seleccione un Panelista:", choices = unique(answers$Valuador)),
            tableOutput("data")

          ),

          )
        )
      )



  # Load panelists answers.
  get_answers <- function() {
    files <- dir(answers_dir, full.names = TRUE)
    # Only keep directories (remove `diseno.csv`).
    files <- files[dir.exists(files)]
    files <- dir(files, full.names = TRUE, recursive = TRUE)
    answers <- data.frame(
      Producto = sub("\\.csv$", "", basename(files)),
      Valuador = basename(dirname(files))
    ) %>%
      bind_cols(read_csv(files, col_types = cols()))
    req(nrow(answers) > 0)
    arrange(answers, Producto, Valuador)%>%
      separate(Producto, c("Producto", "Rep"))
  }

  # Get column types from a data frame.
  col_types <- function(dataset) sapply(dataset, class)



  server <- function(input, output, session) {
    answers <- reactiveVal()

    # Refresh answers every `refresh_seconds` seconds.
    timer <- reactiveTimer(1000 * refresh_seconds)
    observeEvent(timer(), answers(get_answers()))








    # Show panel table.
    output$data <-  renderTable({


      df_final<-crear_stats(answers())
      df_filtrado <- df_final %>%
        filter(Valuador == input$panelista)
      df_filtrado

    })



  }
  # Run the app.
  shinyApp(ui, server, options = list(host = host, port = port))
}
