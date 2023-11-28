# Check up on the testthat results

# Find .git root directory
root_dir <- rprojroot::find_root(rprojroot::has_dir(".git"))

out_file <- list.files(pattern = "testthat.Rout$", file.path(root_dir, "check"),
 recursive = TRUE, full.names = TRUE)

check_content <- readLines(out_file)
test_result <- grep("\\[ FAIL", check_content, value = TRUE)
test_result <- unlist(strsplit(test_result, "\\||\\[|\\]"))

# Format the data into a dataframe
test_result_df <- data.frame(result = trimws(test_result)) %>%
  dplyr::filter(result != "") %>%
  tidyr::separate(result, sep = " ", into = c("test_name", "num")) %>%
  dplyr::mutate(num = as.numeric(num))

if (report_warning) {
  fail_num <- test_result_df %>%
    dplyr::filter(test_name %in% c("FAIL", "WARN"))
} else {
  fail_num <- test_result_df %>%
    dplyr::filter(test_name == "FAIL")
}

fail_num <- as.character(sum(fail_num$fail_num))

# Spit the number out
writeLines(fail_num, con = stdout())

return(fail_num)
