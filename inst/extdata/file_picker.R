token <- get_token("google")

picker_html <- sprintf("<html>
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
          url: doc.url,
          mimeType: doc.mimeType
        }));

        // Write to stdout
        console.log(JSON.stringify(selectedFiles));

        window.close();
      }
    }

    gapi.load('client', onApiLoad);
  </script>
</head>
<body></body>
</html>",
                       token$credentials$access_token,
                       Sys.getenv("GOOGLE_API_KEY"))

temp_html <- tempfile(fileext = ".html")
cat(picker_html, file = temp_html)

# Print out the result
write(temp_html, stdout())
