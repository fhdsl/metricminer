---
title: 'Metricminer:'
tags:
  - R
  - API
  - Google
  - Calendly
  - Slido
  - GitHub
authors:
  - name: Candace Savonen
    orcid: 0000-0001-6331-7070
    corresponding: true
    affiliation: 1
  - name: Kate Isaac
    affiliation: 1
  - name: Howard Baek
    affiliation: 2
  - name: Carrie Wright
    affiliation: 1
  - name: Jeff Leek
    affiliation: 1
affiliations:
 - name: Fred Hutchinson Cancer Center, Seattle, WA, United States
   index: 1
 - name: Yale School of Public Health
   index: 2
date: 12 March 2025
bibliography: paper.bib
---

# Summary

The collection and analysis of standardized metrics is crucial for understanding the true impact and usage patterns of research software, extending far beyond traditional citation counts [@bioinformatics]. Citations alone fail to capture many critical aspects of software utilization, including version information, indirect usage through platforms, and evolving impact over time. "Metric mining" aims to systematically collect and analyze comprehensive usage data, making it accessible to developers, funders, and evaluation committees. There is a vital need for automated, standardized metric collection so that software impact can be demonstrated to funders and potential users. `metricminer` is an open source R package to help address the difficulty and lack of standardization  for metric collection. 

# Statement of need

Previous research by our group showed that the main reason researchers do not collect more metrics is a lack of time and funding to do so [@bioinformatics]. This does create a re-enforcing problem: metrics can help demonstrate impact to funders which would, in turn, give developers more funding to increase the software's impact. Collecting meaningful metrics from Application Programming Interfaces (APIs) can be time consuming, so `metricminer` aims to be an open-source solution to make the metric collection process more efficient.

`metricminer` is an R package designed to simplify the process of collecting metrics from common web services. R provides an ideal environment for data analysis and visualization, while allowing for reproducible research workflows. `metricminer` was designed to provide a consistent and user-friendly interface to various web services including GitHub, Google Analytics, Calendly, Google Forms, and YouTube. The package handles authentication, data retrieval, and transformation of the raw API responses into opinionated tidy data frames, making it immediately useful for analysis [@wickham_tidy_2014].

`metricminer` was developed to serve both researchers, educators, and other practitioners who need to collect and analyze metrics from multiple platforms. It has been specifically designed to support educational initiatives and research projects that require tracking engagement across various platforms. The combination of simplified authentication, consistent data formats, and integration with the R ecosystem makes `metricminer` particularly valuable for organizations that need to generate regular reports or maintain dashboards of their metrics.

The package has been designed to support several common use cases:

1. **Educational Analytics**: Tracking engagement across course materials hosted on various platforms
2. **Research Impact**: Monitoring repository activity and documentation usage
3. **Event Management**: Analyzing registration and attendance patterns through Calendly integration
4. **Website Performance**: Collecting and analyzing Google Analytics metrics
5. **Community Engagement**: Measuring interaction through YouTube statistics and Google Forms responses

# Implementation and Features

The package implements several key features that facilitate metrics collection:

1. **Metric Standardization**: Large and unweildy json files are generally returned from most REST APIs. However this can be intimidating for many users to wrangle into a format that is easier to report or make data visualizations. All metrics returned from  `metricminer` are by default returned in tidy data frame format, making it immediately usable with popular R packages like `dplyr` and `ggplot2`. For cases where the default transformation doesn't meet specific needs, the package provides an argument that will return instead the raw API responses. Ongoing community efforts to understand what metrics should be standard in reports can be discussed and facilitated as a part of `metricminer`.

2. **Unified Authentication**: Through the `authorize()` function, users can authenticate with multiple services using a consistent interface. The package supports both interactive and non-interactive authentication methods, making it suitable for both manual use and automated workflows.

3. **Bulk Retrieval Functions**: Not infrequently, users may want to collect metrics from multiple websites, code bases, or etc. This could prove tricky to combine these data but metricminer has bulk retrieval functions that allow a list of the necessary items to be retrieved and metrics from all the items in the list are returned in one standard, tidy data frame. The package includes wrapper functions for collecting data from multiple sources simultaneously, such as `get_multiple_repos_metrics()` for GitHub repositories and `get_multiple_ga_metrics()` for Google Analytics properties.

# Example usage

The authorize function takes care of all API authorization for any apps attempting to integrate with.

```
library(metricminer)
authorize("github")
```

By running this code it will trigger some menu options:
```
Would you like to store/cache your credentials?

1: Yes cache/store credentials
2: No do not store credentials, I will re-run this authorize() in my next R session
```
Credentials have the option to be cached and can be deleted anytime by running the `delete_creds()` function.
```
You chose to cache your credentials, if you change your mind, run metricminer::delete_creds().
Be careful not to push the cache files to GitHub or share it anywhere.
```
For authorization, the proper pages will be open and instructions given for the user to provide their credentials.
```
On the opened page, scroll down and click 'Generate Token'.
```

The authorization process can be done for GitHub, Google, or Calendly.
After authorization then you can run individual metric mining functions like:

```
metrics <- get_github_repo_timecourse(
  repo = "fhdsl/metricminer"
  )
```

This will return a data frame stored in `metrics` that will look something like this.

|repo|timestamp|count_clones|uniques_clones|count_views|uniques_views|
|---|---|---|---|---|---|
|fhdsl/metricminer-dashboard|2024-04-29|1|2|1|1|
|fhdsl/metricminer-dashboard|2024-04-30|1|1|5|1|
|fhdsl/metricminer-dashboard|2024-05-01|1|1|1|NA|
|fhdsl/metricminer-dashboard|2024-05-05|16|9|3|1|


Each data source has a set of functions for metrics it can retrieve and [all are described here](https://hutchdatascience.org/metricminer/).
metricminer also has a template dashboard that can be used as a starting point and for example usage and plots [@dashboard].

# Future Work

Future development of `metricminer` will focus on further polishing and development of this package and its [template dashboard repository](https://hutchdatascience.org/metricminer-dashboard/). This may include adding support for new sources of metrics depending on user reported needs.

# Acknowledgements

This work was supported by National Cancer Institute grant UE5CA254170.

# References
