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
  - name: Howard Baek
    affiliation: 2
  - name: Kate Isaac
    affiliation: 1
  - name: Carrie Wright
    affiliation: 1
  - name: Jeff Leek
    affiliation: 1
affiliations:
 - name: Fred Hutchinson Cancer Center, Seattle, WA, United States
   index: 1
 - name: Yale School of Public Health
   index: 2
date: 12 February 2025
bibliography: paper.bib
---

# Summary

The collection and analysis of standardized metrics is crucial for understanding the true impact and usage patterns of research software, extending far beyond traditional citation counts [@bioinformatics]. As highlighted by studies of biomedical software, citations alone fail to capture many critical aspects of software utilization, including version information, indirect usage through platforms, and evolving impact over time. The field of "metrics mining" aims to systematically collect and analyze comprehensive usage data, making it accessible to developers, funders, and evaluation committees. However, the current academic infrastructure, built around static manuscript publication, presents significant challenges in creating unified, reproducible analyses of software impact. There is a vital need for automated, standardized metric collection systems that can track software usage across platforms, capture version-specific information, and document indirect utilization through larger frameworks or platforms.

# Statement of need

`metricminer` is an R package designed to simplify the process of collecting metrics from common web services. R provides an ideal environment for data analysis and visualization, while allowing for reproducible research workflows. `metricminer` was designed to provide a consistent and user-friendly interface to various web services including GitHub, Google Analytics, Calendly, Google Forms, and YouTube. The package handles authentication, data retrieval, and transformation of the raw API responses into opinionated tidy data frames, making it immediately useful for analysis [@tidy].

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

3. **Bulk Retrieval Functions**: Not infrequently, users may want to collect metrics from multiple websites, code bases, or etc. This could prove tricky to combine these data but metricminer has bulk retrieval functions that allow a  list of the necessary items to be retrieved and metrics from all the items in the list are returned in one standard, tidy data frame. The package includes wrapper functions for collecting data from multiple sources simultaneously, such as `get_multiple_repos_metrics()` for GitHub repositories and `get_multiple_ga_metrics()` for Google Analytics properties.

# Future Work

Future development of `metricminer` will focus on further polishing and development of this package and its [template dashboard repository](https://hutchdatascience.org/metricminer-dashboard/). This may include adding new sources and features where metricmining is supported by `metricminer`.

# Acknowledgements

# References
