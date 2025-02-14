library(shiny)
library(shinyjs)

jsCode <- readLines("filepicker.js")

shinyApp(
  ui = fluidPage(
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("FilePicker")),
    h1("Google Drive File Picker Example"),
    actionButton("pick", "Pick File"),
    extendShinyjs( "<script>
      function initPicker() {
        var picker = new FilePicker({
          apiKey: 'AIzaSyByRWE_ccYflVJED2g_QLX0qjaQdRDu_E0',
          clientId: 1098881682415-i4djgrp2a7mgn2pdnipco2hs33tqpvq3.apps.googleusercontent.com,
          buttonEl: document.getElementById('pick'),
          onSelect: function(file) {
            console.log(file);
            alert('Selected ' + file.title);
          }
        });
      }
    </script>", functions = c("initPicker"))
  ),
  server = function(input, output) {
    observeEvent(input$col, {
      js$FilePicker(input$col)
    })
  }
)
