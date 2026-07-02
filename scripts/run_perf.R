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


run_perf <- function(answers_dir = "Answers", dest_url = NULL, panel_url = NULL,
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

  ui <- fluidPage(

      mainPanel(
        tabsetPanel(
          tabPanel(
            "Datos",
            h1("Datos"),
            br(),
            tableOutput("data"),
            tableOutput("data2")
          ),
          tabPanel(
            "Reproducibilidad",
            br(),
            tableOutput("answer_repro")
          ),
          tabPanel(
            "estabilidad",
            br(),
            tableOutput("answer_estab")
          ),
          tabPanel(
            "Cajas",
            h1("Cajas"),
            br(),
            selectInput("attribute_selector", "Atributo", NULL),
            plotOutput("answer_box")
          ),
          tabPanel(
            "MFA",
            h1("MFA"),
            br(),
            plotOutput("answer_MFA"),
            tableOutput("answer_mfa")
          ),
          tabPanel(
            "Interacción",
            br(),
            plotOutput("answers_interact",width="800px")
          )
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
      separate(Producto, c("Producto", "Rep"),sep="_")
  }

  # Get column types from a data frame.
  col_types <- function(dataset) sapply(dataset, class)

  # Generates the QR code plot from the `text`.
  qr_gen <- function(text) {
    if (!require("qrcode") | is.null(text)) {
      return(invisible())
    }
    qr_matrix <- qrcode_gen(text, dataOutput = TRUE, plotQRcode = FALSE)
    qr_matrix <- as.data.frame.table(qr_matrix)
    qr_matrix[1:2] <- lapply(qr_matrix[1:2], as.numeric)
    qr_matrix <- qr_matrix[qr_matrix$Freq == 1, ]
    ggplot(qr_matrix, aes(Var1, Var2)) +
      geom_tile() +
      theme_void() +
      theme(aspect.ratio = 1)
  }

  server <- function(input, output, session) {
    answers <- reactiveVal()

    # Refresh answers every `refresh_seconds` seconds.
    timer <- reactiveTimer(1000 * refresh_seconds)
    observeEvent(timer(), answers(get_answers()))

    # Show QR code.
    output$qrcode <- renderPlot(qr_gen(panel_url))

    # Show panel table.
    output$data <- renderTable(answers())
    output$data2 <-  renderTable({
      ans <- answers()
      req(
        nrow(ans) > 0, length(unique(ans$Producto)) > 1, length(unique(ans$Valuador)) > 1,
        sum(col_types(ans) == "numeric") > 0
      )
    table(ans$Valuador, ans$Producto)

    })

    # Create reproducibility index

    output$answer_repro <- renderTable(calcular_indice_reproducibilidad(answers()))

    # Create stability index.
    output$answer_estab <- renderTable(calcular_indice(answers()))
    # Update answers selector.
    observeEvent(answers(), {
      ans <- answers()
      req(nrow(ans) > 0)
      choices <- colnames(ans)[col_types(ans) == "numeric"]
      updateSelectInput(
        session, "attribute_selector",
        choices = choices,
        selected = if_else(
          nchar(input$attribute_selector) > 0, input$attribute_selector, choices[[1]]
        )
      )
      updateSelectInput(
        session, "attribute_selector_2",
        choices = choices, selected = input$attribute_selector
      )
    })

    # Update attribute selectors on change.
    observeEvent(
      input$attribute_selector,
      updateSelectInput(session, "attribute_selector_2", selected = input$attribute_selector)
    )
    observeEvent(
      input$attribute_selector_2,
      updateSelectInput(session, "attribute_selector", selected = input$attribute_selector_2)
    )

    # Selected answer boxplot.
    output$answer_box <- renderPlot({
      ans <- answers()
      req(nrow(ans) > 0 && sum(col_types(ans) == "numeric") > 0)
      ans$Valor <- pull(ans, input$attribute_selector)
      ans %>%
        ggplot(aes(x = Producto, y = Valor, label = Valuador)) +
        geom_boxplot() +
        geom_text(aes(color = Valuador)) +
        theme_bw() +
        xlab("") +
        ylab("") +
        coord_cartesian(ylim = numeric_range) +
        scale_y_continuous(breaks = seq(numeric_range[[1]], numeric_range[[2]])) +
        theme(legend.position = "none")
    })

    # Selected answer MFA.
    output$answer_MFA <- renderPlot({
      ans <- answers()
      req(
        nrow(ans) > 0, length(unique(ans$Producto)) > 1, length(unique(ans$Valuador)) > 1,
        sum(col_types(ans) == "numeric") > 0
      )



      df_long <- ans %>%
        pivot_longer(cols = -c(Producto, Rep, Valuador), names_to = "Variable", values_to = "Valor")

      # Luego, creamos una columna que combine Rep y Variable para distinguir entre Rep 1 y Rep 2
      df_long <- df_long %>%
        mutate(Rep_Variable = paste0(Variable, "_Rep", Rep))

      # Finalmente, pivotamos de vuelta para tener cada combinación de Rep y Variable como columnas separadas
      df_wide <- df_long %>%
        select(-Rep, -Variable) %>%
        pivot_wider(names_from = Rep_Variable, values_from = Valor) %>%
        select(-Valuador) %>%
        group_by(Producto) %>%
        summarise_all(mean, na.rm = TRUE) %>%
        column_to_rownames(var = "Producto")
      mfa<-MFA(df_wide, group=c(22,22),
               name.group=c("Rep1","Rep2"),graph = FALSE)
     mfa$group$RV

      output$answer_mfa <- renderTable(mfa$group$RV, align = c, rownames = TRUE)
      plot( MFA(df_wide, group=c(22,22),
                name.group=c("Rep1","Rep2")), choix = "ind", partial="all")


    })

    # Create answers interact

     output$answers_interact <- renderPlot({
       ans <- answers()
       req(
         nrow(ans) > 0, length(unique(ans$Producto)) > 1, length(unique(ans$Valuador)) > 1,
         sum(col_types(ans) == "numeric") > 0
       )
       res.panelperf <- panelperf(as.data.frame(ans),firstvar=4,formul="~Producto+Valuador+Rep+Producto:Valuador+Producto:Rep+Valuador:Rep",random=F)
       coltable(res.panelperf$p.value[order(res.panelperf$p.value[,1]),],col.lower="green", level.lower = 0.05,cex=0.8)

     })
  }
  # Run the app.
  shinyApp(ui, server, options = list(host = host, port = port))
}
