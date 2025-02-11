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

The collection and analysis of metrics from various web services and platforms is crucial for understanding user engagement, project performance, and overall impact. Traditional methods of collecting such data often require manual extraction or writing custom scripts for each service's API. The field of "metrics mining" aims to automate and streamline this process, making it accessible to researchers, educators, and practitioners. However, the diversity of APIs and data formats presents significant challenges in creating unified, reproducible analyses.

# Statement of need

`metricminer` is an R package designed to simplify the process of collecting metrics from common web services. R provides an ideal environment for data analysis and visualization, while allowing for reproducible research workflows. The API for `metricminer` was designed to provide a consistent and user-friendly interface to various web services including GitHub, Google Analytics, Calendly, Google Forms, and YouTube. The package handles authentication, data retrieval, and transformation of the raw API responses into tidy data frames, making it immediately useful for analysis.

`metricminer` was developed to serve both researchers and practitioners who need to collect and analyze metrics from multiple platforms. It has been specifically designed to support educational initiatives and research projects that require tracking engagement across various platforms. The combination of simplified authentication, consistent data formats, and integration with the R ecosystem makes `metricminer` particularly valuable for organizations that need to generate regular reports or maintain dashboards of their metrics.

The package has been designed to support several common use cases:

1. **Educational Analytics**: Tracking engagement across course materials hosted on various platforms
2. **Research Impact**: Monitoring repository activity and documentation usage
3. **Event Management**: Analyzing registration and attendance patterns through Calendly integration
4. **Website Performance**: Collecting and analyzing Google Analytics metrics
5. **Community Engagement**: Measuring interaction through YouTube statistics and Google Forms responses

# Implementation and Features

The package implements several key features that facilitate metrics collection:

1. **Unified Authentication**: Through the `authorize()` function, users can authenticate with multiple services using a consistent interface. The package supports both interactive and non-interactive authentication methods, making it suitable for both manual use and automated workflows.

2. **Data Format Standardization**: All data is returned in tidy data frame format, making it immediately usable with popular R packages like `dplyr` and `ggplot2`. For cases where the default transformation doesn't meet specific needs, the package provides access to raw API responses.

3. **Bulk Retrieval Functions**: The package includes wrapper functions for collecting data from multiple sources simultaneously, such as `get_multiple_repos_metrics()` for GitHub repositories and `get_multiple_ga_metrics()` for Google Analytics properties.

# Future Work

Future development of `metricminer` will focus on further development of associated dashboard templates for automated reporting, integration with additional web services and platforms. 

# Acknowledgements

# References
