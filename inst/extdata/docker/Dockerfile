FROM rocker/tidyverse:4.0.2
LABEL maintainer="cansav09@gmail.com"
WORKDIR /rocker-build/

COPY install_github.R .
COPY git_token.txt .
COPY github_package_list.tsv .

# Install a package from CRAN
RUN Rscript -e  "options(warn = 2);install.packages( \
    c('remotes', \
      'styler'), \
    repos = 'https://cloud.r-project.org/')"

# Install packages from github
RUN Rscript install_github.R \
  --packages github_package_list.tsv \
  --token git_token.txt



# Set final workdir for commands
WORKDIR /home/rstudio
