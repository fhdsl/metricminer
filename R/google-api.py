virtualenv cansavvy
source cansavvy/bin/activate
cansavvy/bin/pip install google-cloud-api-keys disutils google-api-python-client


pip3 install google-oauth2-tool
pip3 install google-api-python-client
pip3 install pandas

# import packages
from apiclient.discovery import build
from google.oauth2.service_account import Credentials
import pandas as pd
# Locate the json key file generated in the previous step
KEY_FILE_LOCATION = r'.secrets/metricminer-76df1eb313a1.json>'
# Build Analytics Reporting API V4 service object.    
def get_service():
    SCOPES = ['https://www.googleapis.com/auth/analytics.readonly']
    credentials = Credentials.from_service_account_file(
        KEY_FILE_LOCATION, scopes=SCOPES
    )
    service = build(serviceName='analyticsreporting', version='v4', credentials=credentials)
    return service