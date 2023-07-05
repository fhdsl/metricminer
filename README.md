# dasl-analytics

To update Google Analytics data:

```r
library(charm)

clean_slate(path = "app/_charm.R")
assemble(path = "app/_charm.R")
```

How to create `app/data/ga-properties.json`:

- Go [here](https://developers.google.com/analytics/devguides/config/admin/v1/rest/v1alpha/properties/list?apix_params=%7B%22filter%22%3A%22parent%3Aaccounts%2F240624532%22%2C%22pageSize%22%3A200%7D)
 - Click "Execute"
 - Copy/paste resulting JSON
