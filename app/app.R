library(shiny)
library(tidyverse)
library(here)
library(ggplot2)
library(slider)

#################
## Data Import ##
#################

ga_data_ <- here("app", "data", "ga.csv") |> read_csv() |>
  mutate(pagePath = gsub("//", "/", pagePath)) |>
  group_by(Property, pagePath) |> mutate(group_size = n()) |>
  ungroup() |> filter(group_size > 1) |>
  select(Property, date, pagePath, activeUsers, sessions) |>
  mutate(date = as.character(date)) |>
  group_by(Property, date, pagePath) |>
  mutate(activeUsers = sum(activeUsers), sessions = sum(sessions)) |>
  ungroup() |> unique()

######################
## UI Specification ##
######################

ga_ui <- tabPanel(title = "Google Analytics",
                  fluidPage(
                    h2("Google Analytics"),
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("ga_property", label = "Property",
                                    choices = NULL),
                        selectInput("ga_page", label = "Page",
                                    choices = NULL),
                        dateRangeInput("ga_date", label = "Date Range",
                                       start = ga_data_$date |> as.Date() |> min(),
                                       end = ga_data_$date |> as.Date() |> max())
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot", br(), plotOutput("ga_dplot"), plotOutput("ga_wplot")),
                                    tabPanel("Table", tableOutput('ga_tbl')),
                                    tabPanel("Summary", tableOutput('ga_summary')),
                        )
                      )
                    )
                  )
)

slack_ui <- tabPanel(title = "FH Data Slack",
                         fluidPage(
                           h2("FH Data Slack"),
                           sidebarLayout(
                             sidebarPanel(
                               p("Under Construction")
                             ),
                             mainPanel(
                               p("Under Construction")
                             )
                           )
                         )
)

calendly_ui <- tabPanel(title = "Calendly",
                        fluidPage(
                          h2("Calendly"),
                          sidebarLayout(
                            sidebarPanel(
                              p("Under Construction")
                            ),
                            mainPanel(
                              p("Under Construction")
                            )
                          )
                        )
)

notion_ui <- tabPanel(title = "Notion",
                        fluidPage(
                          h2("Notion"),
                          sidebarLayout(
                            sidebarPanel(
                              p("Under Construction")
                            ),
                            mainPanel(
                              p("Under Construction")
                            )
                          )
                        )
)

substack_ui <- tabPanel(title = "Substack",
                      fluidPage(
                        h2("Substack"),
                        sidebarLayout(
                          sidebarPanel(
                            p("Under Construction")
                          ),
                          mainPanel(
                            p("Under Construction")
                          )
                        )
                      )
)

dashboards_ui <- navbarMenu(title = "Dashboards",
                            ga_ui,
                            slack_ui,
                            calendly_ui,
                            notion_ui,
                            substack_ui,
                            )

ui <- navbarPage(title = "DaSL Analytics",
                 dashboards_ui)

##########################
## Server Specification ##
##########################

server <- function(input, output, session) {
  # Dynamically populates Property input from data
  updateSelectInput(session = session, inputId = "ga_property",
                    choices = ga_data_ |> pull(Property) |> unique())

  # Dynamically populates Page input based on selected Property
  observeEvent(input$ga_property,
               updateSelectInput(session = session, inputId = "ga_page",
                                 choices = ga_data_ |>
                                   filter(Property == input$ga_property) |>
                                   pull(pagePath) |> unique() |> sort()))

  # Dynamically creates plot for daily metrics based on Property, Page, and
  # Date Range
  ga_daily_plot <- reactive({
    ga_data_ |>
      filter(Property == input$ga_property) |>
      filter(pagePath == input$ga_page) |>
      select(date, activeUsers, sessions) |>
      mutate(date = as.Date(date)) |>
      filter(date >= input$ga_date[1], date <= input$ga_date[2]) |>
      rename(Date = date, "Active Users" = activeUsers, Sessions = sessions) |>
      pivot_longer(cols = c("Active Users", "Sessions"),
                   names_to = "Metric", values_to = "Value") |>
      ggplot(aes(Date, Value, colour = Metric)) +
      geom_line() +
      ggtitle(paste("Daily Google Analytics for", input$ga_property, input$ga_page))
  })

  observeEvent(ga_daily_plot(),
               output$ga_dplot <- renderPlot(
                 ga_daily_plot()
               ))

  # Dynamically creates plot for weekly metrics based on Property, Page, and
  # Date Range
  ga_weekly_plot <- reactive({
    ga_data_ |>
      filter(Property == input$ga_property) |>
      filter(pagePath == input$ga_page) |>
      select(date, activeUsers, sessions) |>
      mutate(date = as.Date(date)) |>
      filter(date >= input$ga_date[1], date <= input$ga_date[2]) |>
      arrange(date) |>
      mutate(activeUsers = slide_dbl(activeUsers, mean, .before = 30)) |>
      mutate(sessions = slide_dbl(sessions, mean, .before = 30)) |>
      rename(Date = date, "Active Users" = activeUsers, Sessions = sessions) |>
      pivot_longer(cols = c("Active Users", "Sessions"),
                   names_to = "Metric", values_to = "Value") |>
      ggplot(aes(Date, Value, colour = Metric)) +
      geom_line() +
      ggtitle(paste("Average Monthly Google Analytics for", input$ga_property, input$ga_page))
  })

  observeEvent(ga_weekly_plot(),
               output$ga_wplot <- renderPlot(
                 ga_weekly_plot()
               ))

  # Dynamically creates table for daily metrics based on Property, Page, and
  # Date Range
  ga_daily_table <- reactive({
    ga_data_ |>
      filter(Property == input$ga_property) |>
      filter(pagePath == input$ga_page) |>
      filter(date >= input$ga_date[1], date <= input$ga_date[2]) |>
      select(date, activeUsers, sessions) |>
      arrange(desc(date)) |>
      rename(Date = date, "Active Users" = activeUsers, Sessions = sessions)
  })

  observeEvent(ga_daily_table(),
            output$ga_tbl <- renderTable(
              ga_daily_table()
            ))

  # Dynamically creates table for most popular Pages based on Property and
  # Date Range
  ga_summary <- reactive({
    ga_data_ |>
      mutate(date = as.Date(date)) |>
      filter(Property == input$ga_property) |>
      filter(date >= input$ga_date[1], date <= input$ga_date[2]) |>
      group_by(pagePath) |>
      summarize("Total Active Users" = sum(activeUsers),
                "Total Sessions" = sum(sessions)) |>
      ungroup() |>
      arrange(desc(`Total Active Users`)) |>
      rename(Page = pagePath)
  })

  observeEvent(ga_summary(),
               output$ga_summary <- renderTable(
                 ga_summary()
               ))
}

shinyApp(ui, server)
