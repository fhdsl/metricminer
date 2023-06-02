library(shiny)
library(tidyverse)
library(here)
library(ggplot2)

ga_data_ <- here("data", "ga.csv") |> read_csv() |>
  mutate(pagePath = gsub("//", "/", pagePath)) |>
  group_by(Property, pagePath) |> mutate(group_size = n()) |>
  ungroup() |> filter(group_size > 1) |>
  select(Property, date, pagePath, activeUsers, sessions) |>
  mutate(date = as.character(date)) |>
  group_by(Property, date, pagePath) |>
  mutate(activeUsers = sum(activeUsers), sessions = sum(sessions)) |>
  ungroup() |> unique()

ga_ui_bdsw <- tabPanel(title = "other",
                         fluidPage(
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("status", label = "Status of Purchase:",
                                           choices = c("Purchased", "Cancelled")),
                             ),
                             mainPanel(
                               # tableOutput('ga_tbl')
                             )
                           )
                         )
)

ga_ui <- tabPanel(title = "Google Analytics",
                  fluidPage(
                    h2("Google Analytics"),
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("ga_property", label = "Property",
                                    choices = NULL),
                        selectInput("ga_page", label = "Page",
                                  choices = NULL),
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                          tabPanel("Plot", plotOutput("ga_plot")),
                          tabPanel("Table", tableOutput('ga_tbl')),
                        )
                      )
                    )
                  )
)

ga_ui <- navbarMenu(title = "Dashboards",
                    ga_ui, ga_ui_bdsw)

ui <- navbarPage(title = "DaSL Analytics",
                 ga_ui)

server <- function(input, output, session) {
  updateSelectInput(session = session, inputId = "ga_property",
                    choices = ga_data_ |> pull(Property) |> unique())

  observeEvent(input$ga_property,
               updateSelectInput(session = session, inputId = "ga_page",
                                 choices = ga_data_ |>
                                   filter(Property == input$ga_property) |>
                                   pull(pagePath) |> unique() |> sort()))

  observeEvent(input$ga_page,
            output$ga_tbl <- renderTable(
              ga_data_ |>
                filter(Property == input$ga_property) |>
                filter(pagePath == input$ga_page) |>
                select(date, activeUsers, sessions) |>
                arrange(desc(date))
            ))

  observeEvent(input$ga_page,
               output$ga_plot <- renderPlot(
                 ga_data_ |>
                   filter(Property == input$ga_property) |>
                   filter(pagePath == input$ga_page) |>
                   select(date, activeUsers, sessions) |>
                   mutate(date = as.Date(date)) |>
                   rename(Date = date) |>
                   pivot_longer(cols = c("activeUsers", "sessions"),
                                names_to = "Metric", values_to = "Value") |>
                   ggplot(aes(Date, Value, colour = Metric)) +
                    geom_line()
               ))
}

shinyApp(ui, server)
