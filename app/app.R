library(shiny)
library(tidyverse)
library(here)
library(ggplot2)
library(slider)

#################
## Data Import ##
#################

here_ <- function(...) {
  # Am I on Fred Hutch infrastructure?
  on_fh_infra <- getwd() == "/home/shiny" &&
    as.character(Sys.info()["user"]) == "shiny"

  if (on_fh_infra) {
    here(...)
  } else {
    here("app", ...)
  }
}

ga_data_ <- here_("data", "ga.csv") |> read_csv() |>
  mutate(pagePath = gsub("//", "/", pagePath)) |>
  group_by(Property, pagePath) |> mutate(group_size = n()) |>
  ungroup() |> filter(group_size > 1) |>
  select(Property, date, pagePath, activeUsers, sessions) |>
  mutate(date = as.character(date)) |>
  group_by(Property, date, pagePath) |>
  mutate(activeUsers = sum(activeUsers), sessions = sum(sessions)) |>
  ungroup() |> unique()

slack_data <- here_("data", "slack.csv") |> read_csv() |>
  select(Date, "Total Members", "Daily active members",
         "Daily members posting messages", "Messages in public channels",
         "Messages in private channels", "Messages in DMs")

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
                               dateRangeInput("slack_date", label = "Date Range",
                                              start = slack_data$Date |> as.Date() |> min(),
                                              end = slack_data$Date |> as.Date() |> max())
                             ),
                             mainPanel(
                               plotOutput("slack_total_members_plot"),
                               plotOutput("slack_weekly_members_plot"),
                               plotOutput("slack_messages_plot")
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
      ggtitle(paste("Daily Google Analytics for", input$ga_property, input$ga_page)) +
      theme_minimal()
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
      ggtitle(paste("Average Monthly Google Analytics for", input$ga_property, input$ga_page)) +
      theme_minimal()
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

  # Dynamically creates a data for Slack based on date range
  slack_data_by_date <- reactive({
    slack_data |>
      filter(Date >= input$slack_date[1], Date <= input$slack_date[2])
  })

  output$slack_total_members_plot <- renderPlot(
    slack_data_by_date() |>
      ggplot(aes(Date, `Total Members`)) +
        geom_line() +
        ggtitle("FH Data Slack Total Membership") +
        theme_minimal()
  )

  output$slack_weekly_members_plot <- renderPlot(
    slack_data_by_date() |>
      select(Date, "Daily active members", "Daily members posting messages") |>
      arrange(Date) |>
      mutate(`Daily active members` = slide_dbl(`Daily active members`, mean, .before = 30)) |>
      mutate(`Daily members posting messages` = slide_dbl(`Daily members posting messages`, mean, .before = 30)) |>
      rename(`Members posting messages` = `Daily members posting messages`,
             `Active members` = `Daily active members`) |>
      pivot_longer(cols = c("Active members", "Members posting messages"),
                   names_to = "Metric", values_to = "Value") |>
      ggplot(aes(Date, Value, colour = Metric)) +
      geom_line() +
      ggtitle("FH Data Slack Average Monthly Membership Activity") +
      theme_minimal()
  )

  output$slack_messages_plot <- renderPlot(
    slack_data_by_date() |>
      select(Date, "Messages in public channels", "Messages in private channels",
             "Messages in DMs") |>
      arrange(Date) |>
      mutate(`Messages in public channels` = slide_dbl(`Messages in public channels`, mean, .before = 30)) |>
      mutate(`Messages in private channels` = slide_dbl(`Messages in private channels`, mean, .before = 30)) |>
      mutate(`Messages in DMs` = slide_dbl(`Messages in DMs`, mean, .before = 30)) |>
      pivot_longer(cols = c("Messages in public channels",
                            "Messages in private channels",
                            "Messages in DMs"),
                   names_to = "Metric", values_to = "Value") |>
      ggplot(aes(Date, Value, colour = Metric)) +
      geom_line() +
      ggtitle("FH Data Slack Average Monthly Message Activity") +
      theme_minimal()
  )
}

shinyApp(ui, server)
