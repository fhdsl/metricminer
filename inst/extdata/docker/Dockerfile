FROM rocker/tidyverse:4.3
LABEL maintainer="cansav09@gmail.com"
WORKDIR /rocker-build/

COPY install_github.R .
COPY git_token.txt .
COPY github_package_list.tsv .

# Install packages from github
RUN Rscript install_github.R \
  --packages github_package_list.tsv \
  --token git_token.txt

RUN R -q -e 'install.packages("cranlogs", repos="https://cran.rstudio.com/")'

# Set final workdir for commands
WORKDIR /home/rstudio
