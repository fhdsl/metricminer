# metricminer <img src="https://raw.githubusercontent.com/fhdsl/metricminer/main/resources/metricminer-gnome.png" width = "100">

Digging up data that matters, making it dashboard-ready.

`metricminer` is an R package that helps you mine metrics from common places on the web through the power of their APIs.

It also helps format the data so that it can easily be used for a dashboard or other purposes.
It will have an associated [dashboard template](https://github.com/fhdsl/metricminer-dashboard) and tutorials to help you fully utilize the data you retrieve with `metricminer`  (but these are still under development!)

- You can [read the metricminer package documentation here](https://hutchdatascience.org/metricminer/).
- Additionally, you can read more about metric collection in our [associated manuscript, which is currently a preprint](https://arxiv.org/abs/2306.03255).

## Apps Supported

Currently `metricminer` supports mining data from:

- [Calendly](https://calendly.com/)
- [GitHub](https://github.com/)
- [Google Analytics](https://developers.google.com/analytics)
- [Google Forms](https://www.google.com/forms/about/)
- [Youtube](https://www.youtube.com/)
- [Slido](https://admin.sli.do/events) export files stored on Googledrive

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [metricminer](#metricminer)
  - [Apps supported](#apps-supported)
  - [Data format options](#data-format-options)
  - [How to install](#how-to-install)
  - [Basic Usage](#basic-usage)
    - [GitHub](#github)
    - [Calendly](#calendly)
    - [Google Analytics](#google-analytics)
    - [Google Forms](#google-forms)
    - [Slido](#slido)
    - [Youtube](#youtube)
  - [Bulk Retrievals](#bulk-retrievals)
    - [GitHub bulk](#github-bulk)
    - [Google Analytics bulk](#google-analytics-bulk)
    - [Google Forms](#google-forms-1)
  - [Contributions](#contributions)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data Format Options

`metricminer`  retrieves API data for you and gives it to you in a format that is a tidy data frame. This means metricminer has to be selective about what metrics it returns, ensuring it fits in a useful data frame that can be easily read by humans.

If you find that the data returned is not what you need, you have two options (these options can be pursued concurrently):

1. You can set the `dataformat` argument to `"raw"` to see the original, unedited JSON formatted data as it was returned from the API. Then you can personally look for the data that you want and extract it.
2. You can post a GitHub issue to explain why the metric missing from the data frame of formatted data should be included. And if possible and reasonable, we can work on including that data in the next version of `metricminer`.

## How to install

If you want the development version (not advised) you can install using the `remotes` package to install from GitHub.
``` r
if (!("remotes" %in% installed.packages())) {
  install.packages("remotes")
}
remotes::install_github("fhdsl/metricminer")
```

Attach the library or decide to use the `metricminer::` notation.
```{r setup}
library(metricminer)
```

## Basic Usage

To start, you need to `authorize()` the package to access your data. If you run `authorize()` you will be asked which app you'd like to authorize and whether you'd like to cache that authorization information. If you already know which app you'd like to authorize, like `google` for example, you can run `authorize("google")`.

Then follow the instructions on the upcoming screens and select the scopes you feel comfortable sharing (you generally just need read permissions for metricminer to be able to collect data).

```r
authorize()
```

If you want to clear out authorizations and caches stored by `metricminer` you can run:

```
delete_creds()
```

### GitHub

You can retrieve metrics from a repository on GitHub by running:
```
authorize("github")
metrics <- get_github_metrics(repo = "fhdsl/metricminer")
```

### Calendly

You can retrieve Calendly events information using this type of workflow:
```
authorize("calendly")
user <- get_calendly_user()
events <- list_calendly_events(user = user$resource$uri)
```

### Google Analytics

You can retrieve Google Analytics data for websites like this.

First you have to retrieve your account information after you've authorized.

```
authorize("google")
accounts <- get_ga_user()
```

Then you need to retrieve the properties (a.k.a usually the websites you are tracking)
underneath that account.

```
properties_list <- get_ga_properties(account_id = accounts$id[1])
```

Just need to shave off the `properties/` bit from this string.

```
property_id <- gsub("properties/", "", properties_list$properties$name[1])
```

Now we can collect some stats.

In Google Analytics, `metrics` are your basic numbers (how many visits to your website, etc.).
```
metrics <- get_ga_stats(property_id, stats_type = "metrics")
```
Whereas `dimensions` are more  a list of events that have happened. So here's a list of people that have logged on.
```
dimensions <- get_ga_stats(property_id, stats_type = "dimensions")
```
Lastly, we have a third option of collecting `link_clicks` and the links they have clicked. This is also known as a dimension according to Google Analytics. However it often isn't compatible for us to download data about link clicks at the same time as other dimension data so in `metricminer` we collect them separately.
```
link_clicks <- get_ga_stats(property_id, stats_type = "link_clicks")
```

### Google Forms

You can retrieve Google Forms information and responses like this:
```
authorize("google")
form_url <- "https://docs.google.com/forms/d/1Z-lMMdUyubUqIvaSXeDu1tlB7_QpNTzOk3kfzjP2Uuo/edit"
form_info <- get_google_form(form_url)
```

### Slido

If you have used Slido for interactive slide sessions and collected that info and exported it to your Google Drive, you can use `metricminer` to collect that data as well.

```
drive_folder_id <- "1FvGsZ5M8E6o17K5pI6HNGM-F1xwS9ML1"
slido_data <- get_slido_files(drive_folder_id)
```
### YouTube

If you have a YouTube channel and the URL is https://www.youtube.com/watch?v=oMVVeZjHJ48

Then you can extract stats for the videos on that YouTube channel using that URL.
```
authorize("google")
youtube_video_stats <- get_youtube_video_stats("oMVVeZjHJ48")
youtube_playlist_stats <- get_youtube_playlist_stats("PL9bqxQvtZgAMblZJhg7e0_ThDD-pN4UqA")
```

## Bulk Retrievals

Maybe you just want to retrieve it ALL. We have some wrapper functions that will attempt to do this for you.
These functions are a bit more precarious/risky in that there may be reasons certain websites/repos/events/data may not be able to be collected. So collecting repositories one by one will allow you more insight into what is happening.

However, these bulk retrieval functions may help you if you want to grab ALL of your accounts data in one swoop. Just make sure to carefully look over and curate that data after it is attempted to be collected. You may find some retrievals are empty for potentially good reasons (for example if a google form has no responses to collect it will show up with "no responses" in the respective part of the list).

### GitHub bulk

From GitHub you can attempt to collect repository metrics from all repositories from an account.

```
authorize("github")
```

If you want to do this by giving a list of specific repositories you want data from you can just provide a vector of those repository's names like this:
```
repo_names <- c("fhdsl/metricminer", "jhudsl/OTTR_Template")
some_repos_metrics <- get_multiple_repos_metrics(repo_names = repo_names)
```

If you want the time course related data, you can set `time_course = TRUE`.

```
some_repos_metrics <- get_multiple_repos_metrics(repo_names = repo_names, time_course = TRUE)
```

### Google Analytics bulk

Similar to single website retrieval we need to authorize the package.
```
authorize("google")
accounts <- get_ga_user()
```

Then we can provide the account id to `all_ga_metrics` and it will attempt to grab all stats for all website properties underneath the provided account.

```
stats_list <- all_ga_metrics(account_id = accounts$id[5])
```


### Google Forms

As always, we need to authorize the app.
```
authorize("google")
```

We can retrieve a list of form ids using `googledrive` R package.
```
form_list <- googledrive::drive_find(
  shared_drive = googledrive::as_id("0AJb5Zemj0AAkUk9PVA"),
  type = "form")
```

Now we can provide this vector of form ids to `get_multiple_forms`
```
multiple_forms <- get_multiple_forms(form_ids = form_list$id)
```

## Contributions

This is an ever-evolving package. contact csavonen@fredhutch.org if you are interested in helping us develop `metricminer`, or just file a pull request or issue!
