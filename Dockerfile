FROM fredhutch/r-shiny-base:4.2.0
RUN Rscript -e "install.packages(c('tidyverse', 'here', 'ggplot2', 'slider'))"
RUN apt-get update
RUN apt-get install -y pandoc
RUN useradd -u 5555 -m -d /home/shiny -c "shiny user" shiny
ADD app/. /home/shiny/
RUN chown -R shiny:shiny /home/shiny
WORKDIR /home/shiny
USER shiny
EXPOSE 7777
CMD Rscript start.R
