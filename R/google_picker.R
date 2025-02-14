launch_picker <- function() {
  token <- get_token("google")

  picker_html <- sprintf("
    <html>
    <head>
      <script src=\"https://apis.google.com/js/api.js\"></script>
      <script src=\"https://accounts.google.com/gsi/client.js\"></script>
      <script>
        let picker;
        let pickerInited = false;
        let result = null;

        function onApiLoad() {
          gapi.load('picker', function() {
            pickerInited = true;
            createPicker();
          });
        }

        function createPicker() {
          const picker = new google.picker.PickerBuilder()
            .addView(google.picker.ViewId.DOCS)
            .setOAuthToken('%s')
            .setDeveloperKey('%s')
            .setCallback(handlePickerResult)
            .build();

          picker.setVisible(true);
        }

        function handlePickerResult(data) {
          if (data.action === google.picker.Action.PICKED) {
            const selectedFiles = data.docs.map(doc => ({
              id: doc.id,
              name: doc.name,
              url: doc.url
            }));

            // Store the result
            result = selectedFiles;

            // Write the result to standard output
            System.out.println(result);

            // Close the picker window
            window.close();
          }
        }

        gapi.load('client', onApiLoad);
      </script>
    </head>
    <body></body>
    </html>
  ", token$credentials$access_token, Sys.getenv("GOOGLE_API_KEY"))

  temp_file <- tempfile(fileext = ".html")
  cat(picker_html, file = temp_file)

  # Launch browser and wait for user selection
  browseURL(temp_file)

  # Return the result
  return(result)
}
