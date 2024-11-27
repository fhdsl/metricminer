FROM rocker/tidyverse:4.4
LABEL maintainer="cansav09@gmail.com"
WORKDIR /rocker-build/

# Install packages from github
RUN installGithub.r fhdsl/metricminer

RUN R -q -e 'install.packages("cranlogs", repos="https://cran.rstudio.com/")'

# Set final workdir for commands
WORKDIR /home/rstudio
