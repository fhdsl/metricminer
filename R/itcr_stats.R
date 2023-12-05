# C. Savonen
# Dec 2023
# A first quick stab for collecting metrics on ITN happenings.

authorize("google")

## collect googl analytics
fhdsl_stats_list <- all_ga_metrics(account_id = accounts$items$id[1])
itcr_stats_list <- all_ga_metrics(account_id = accounts$items$id[2])

all_data <- dplyr::bind_rows(fhdsl_stats_list$metrics ,itcr_stats_list$metrics)

# Collect coursera numbers by hand
coursera_numbers <- c(
  "AI in Software" = 72,
  "Leadership Course" = 605,
  "Documentation and Usability Course" = 275,
  "Computing" = 893,
  "Reproducibility" = 347,
  "Advanced Reproducibility in Cancer Informatics" = 255
) %>%
  data.frame() %>%
  tibble::rownames_to_column("website") %>%
  dplyr::rename("total_coursera_enrollments"= ".")

# Collect leanpub numbers by hand
leanpub_numbers <- c(
  "AI in Software" = 6,
  "Leadership Course" = 32,
  "Documentation and Usability Course" = 25,
  "Computing" = 19,
  "Reproducibility" = 12,
  "Advanced Reproducibility in Cancer Informatics" = 15,
  "Choosing Genomic Tools" = 11
) %>%
  data.frame() %>%
  tibble::rownames_to_column("website") %>%
  dplyr::rename("total_leanpub_enrollees"= ".")


# Filter out non ITCR websites
itcr_data <- all_data %>%
  dplyr::filter(!(website %in% c("hutchdatasci", "whoiswho", "MMDS", "FH Cluster 101"))) %>%
  dplyr::left_join(coursera_numbers) %>%
  dplyr::left_join(leanpub_numbers)

readr::write_tsv(itcr_data, "itcr_website_metrics.tsv")

# Collect loqui data
loqui_usage <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1G_HTU-bv2k5txExP8EH3ScUfGqtW1P3syThD84Z-g9k/edit#gid=0")
loqui_usage %>% count(email)

## Collect slido info
slido_data <- readRDS("slido_data.RDS")
slido_data$JoinedParticipants %>% dplyr::count(event_name)


### PLOTS

## Bookdown
ggplot2::ggplot(itcr_data, ggplot2::aes(x = reorder(website, -activeUsers), y = activeUsers)) +
  ggplot2::geom_bar(stat = "identity", fill = "#89CFF0") +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1)) +
  ggplot2::xlab("") +
  ggplot2::geom_text(ggplot2::aes(label = activeUsers), size = 3, vjust = - 1) +
  ggplot2::ylim(c(0, 5500))

## Coursera
ggplot2::ggplot(itcr_data, ggplot2::aes(x = reorder(website, -total_coursera_enrollments), y = total_coursera_enrollments)) +
  ggplot2::geom_bar(stat = "identity", fill = "#89CFF0", na.rm = TRUE) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1)) +
  ggplot2::xlab("") +
  ggplot2::ylab("Coursera enrollments") +
  ggplot2::geom_text(ggplot2::aes(label = total_coursera_enrollments), size = 3, vjust = - 1, na.rm = TRUE) +
  ggplot2::ylim(c(0, 1200))

## Leanpub
ggplot2::ggplot(itcr_data, ggplot2::aes(x = reorder(website, -total_leanpub_enrollees), y = total_leanpub_enrollees)) +
  ggplot2::geom_bar(stat = "identity", fill = "#89CFF0", na.rm = TRUE) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1)) +
  ggplot2::xlab("") +
  ggplot2::ylab("Leanpub enrollments") +
  ggplot2::geom_text(ggplot2::aes(label = total_leanpub_enrollees), size = 3, vjust = - 1, na.rm = TRUE) +
  ggplot2::ylim(c(0, 40))

## Sum the learners up
sum(itcr_data$activeUsers, na.rm = TRUE)
sum(itcr_data$total_coursera_enrollments, na.rm = TRUE)
sum(itcr_data$total_leanpub_enrollees, na.rm = TRUE)
