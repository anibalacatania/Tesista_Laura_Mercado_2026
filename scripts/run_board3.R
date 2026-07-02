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


run_board3 <- function(answers_dir = "Answers", dest_url = NULL, panel_url = NULL,
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
            tableOutput("data")
          ),
          tabPanel(
            "ACP",
            h1("ACP"),
            br(),
            plotOutput("answer_acp")
          ),
          tabPanel(
            "ACP2",
            h1("ACP"),
            br(),
            plotOutput("answer_acp2")
          ),
          tabPanel(
            "Cajas",
            h1("Cajas"),
            br(),
            selectInput("attribute_selector", "Atributo", NULL),
            plotOutput("answer_box")
          ),
          tabPanel(
            "Anova",
            h1("Anova"),
            br(),
            selectInput("attribute_selector_2", "Atributo", NULL),
            plotOutput("answer_anova")
          ),
          tabPanel(
            "Radar",
            h1("Radar"),
            br(),
            plotOutput("answers_radar")
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
      separate(Producto, c("Producto", "Rep"),sep="_")%>%
      separate(Producto, c("Botella", "Producto"),sep="-")
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

    # Create answers radar plot.
    # Create answers radar plot.
    output$answer_acp <- renderPlot({
      ans <- answers()
      req(nrow(ans) > 0 && sum(col_types(ans) == "numeric") > 0)
      ans <- select(ans, Producto, Valuador, where(is.numeric)) %>%
        as.data.frame()


      plot <- panellipse(ans,col.p=1,col.j=2,firstvar=3,level.search.desc=1,graph.type = "ggplot")
      plot1<- plot$graph$plotIndEll+
        theme(aspect.ratio = 1) +labs(title="")+
        theme(axis.title.y = element_text(hjust = 0.5))+
        theme(axis.title.x = element_text(hjust = 0.5))+

        stat_ellipse(type = "t")

      plot2<-plot$graph$plotVarVariab+ theme(legend.position = "none")+
        labs(title="")+
        theme(axis.title.y = element_text(hjust = 0.5))+
        theme(axis.title.x = element_text(hjust = 0.5))
      grid.arrange(plot1, plot2, ncol=2)
    })
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

    # Selected answer anova.
    output$answer_anova <- renderPlot({
      ans <- answers()
      req(
        nrow(ans) > 0, length(unique(ans$Producto)) > 1, length(unique(ans$Valuador)) > 1,
        sum(col_types(ans) == "numeric") > 0
      )
      aov <- aov(pull(ans, input$attribute_selector) ~ Producto+Valuador+Rep+Producto:Valuador+Producto:Rep+Valuador:Rep, data = ans)
      lsd <- LSD.test(aov, "Producto", p.adj = "bonferroni")
      bar.group(lsd$groups, ylim = numeric_range, col="blue", border = "blue", density=30,cex.names = 0.8,angle=90)
    })

    # Create answers radar plot.
    output$answers_radar <- renderPlot({
      ans <- answers()
      req(nrow(ans) > 0 && sum(col_types(ans) == "numeric") > 0)
      select(ans, -Valuador) %>%
        group_by(Producto) %>%
        summarise_if(is.numeric, median) %>%
        ggradar(
          grid.min = numeric_range[[1]], grid.max = numeric_range[[2]], values.radar = c("", "", "")
        )
    })
  }

  # Run the app.
  shinyApp(ui, server, options = list(host = host, port = port))
}
