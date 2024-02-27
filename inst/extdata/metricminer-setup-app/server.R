
  server <- function(input, output) {

    output$github_repo <- renderText({

       # Split it up
       split_it <- strsplit(input$github_repo, split = "\\/")
       owner <- split_it[[1]][1]
       repo <- split_it[[1]][2]
    })

    output$google_refresh <- renderText({
      input$google_refresh
      })

    output$google_access <- renderText({
      input$google_access
    })

    output$github <- renderText({
      input$github
    })

    output$calendly <- renderText({
      input$calendly
    })
  }

