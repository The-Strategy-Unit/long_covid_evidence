library(tidyverse)
library(readxl)
library(janitor)

library(shiny)
library(shinydashboard)
library(DT)

# prep data ----
long_covid_evidence_path <- "long_covid_evidence.xlsx"

long_covid_evidence <- long_covid_evidence_path %>%
    read_excel("Understanding the condition") %>%
    clean_names() %>%
    mutate(id = row_number(), .before = everything(),
           across(month, map_chr, ~ifelse(.x == "n.d.", "(n.d.)", month.abb[as.numeric(.x)])),
           date = paste(month, year, "-"))

# build ui ----
ui <- function() {
    header <- dashboardHeader(
        title = "Long Covid Evidence Map"
    )

    header$children[[3]]$children[[3]]$children[[1]] <- tags$img(
        src = "https://www.strategyunitwm.nhs.uk/themes/custom/ie_bootstrap/logo.svg",
        title = "The Strategy Unit",
        alt = "The Strategy Unit Logo",
        align = "right",
        height = "40"
    )

    siderbar <- dashboardSidebar(
        sidebarMenu(
            selectInput("theme", "Theme", c("all", sort(unique(long_covid_evidence$theme)))),
            selectInput("group", "Group", c("all", sort(unique(long_covid_evidence$evidence_group))))
        )
    )

    body <- dashboardBody(
        box(
            title = "Evidence",
            width = 12,
            DTOutput("evidence")
        )
    )

    tagList(
        includeCSS("skin-su.css"),
        dashboardPage(header, siderbar, body)
    )
}

# build server ----
server <- function(input, output, session) {
    evidence <- reactive({
        theme <- req(input$theme)
        group <- req(input$group)

        long_covid_evidence %>%
            filter(.env$theme == "all" | .data$theme == .env$theme,
                   .env$group == "all" | .data$evidence_group == .env$group) %>%
            select(.data$id,
                   .data$evidence_type,
                   .data$author,
                   .data$title,
                   .data$date,
                   .data$publication_stage,
                   .data$country,
                   .data$timeframe)
    })

    output$evidence <- renderDT(
        evidence()
    )

    observeEvent(input$evidence_rows_selected, {
        # get the currently selected row
        row <- req(input$evidence_rows_selected)
        e <- filter(long_covid_evidence, .data$id == row)

        showModal(
            modalDialog(
                title = e$title,

                tags$strong(e$author),
                " (",
                e$country,
                ") ",
                tags$a("[source]", href = e$link),
                " ",
                tags$br(),

                tags$strong("Publication Stage:"),
                " ",
                e$publication_stage,
                tags$br(),

                tags$strong("Population Groups Concerned:"),
                " ",
                e$population_groups_concerned,
                tags$br(),

                tags$strong("Timeframe:"),
                " ",
                e$timeframe,
                tags$br(),
                tags$br(),

                tags$strong("Abstract:"),
                tags$br(),
                e$description_of_source_abstract
            )
        )

        # clear the selected row
        p <- dataTableProxy("evidence")
        isolate({
            selectRows(p, NULL)
        })
    })
}

# Run the application ----
shinyApp(ui = ui(), server = server)
