FROM jhudsl/base_ottr:main
LABEL maintainer="cansav09@gmail.com"

# Install python
RUN apt-get -y --no-install-recommends install \
    python3-pip  python3-dev

# Install python packages
RUN pip3 install \
   google-cloud-api-keys \
   disutils \
   google-oauth2-tool \
   google-api-python-client
