## Privacy Policy  

Last updated: February 13, 2024

Adapted from https://www.tidyverse.org/google_privacy_policy/

## Privacy policy for packages that access Google APIs
metricminer is an R package wrapper for Google data as well as other web services. This includes obtaining data from the APIs from these Google products:

- Google forms
- Googledrive
- Googlesheets
- Google Analytics
- Youtube

All of these packages are governed by common policies recorded here. These packages use internal resources owned by the “metricminer” project on Google Cloud Platform. That is the name you will see in a consent screen.

Your use of Google APIs with these packages is subject to each API’s respective terms of service. See https://developers.google.com/terms/.

## Privacy

### Google account and user data

#### Accessing user data

These packages access Google resources from your local machine. Your machine communicates directly with the Google APIs.

The `metricminer` project never receives your data or the permission to access your data. The owners of the project can only see anonymous, aggregated information about usage of tokens obtained through its OAuth client, such as which APIs and endpoints are being used.

Each package includes functions that you can execute in order to read or modify your own data. This can only happen after you provide a token, which requires that you authenticate yourself as a specific Google user and authorize these actions.

These packages can help you get a token by guiding you through the OAuth flow in the browser. There you must consent to allow the `metricminer` to operate on your behalf. The OAuth consent screen will describe the scope of what is being authorized, e.g., it will name the target API(s) and whether you are authorizing “read only” or “read and write” access.

### Scopes
Overview of the scopes as they are seen on the OAuth screen, requested by various metricminer and the rationale of why you may need them:

- `See, edit, create, and delete all of your Google Drive files.` This is used for if you'd like store metrics you collect with metricminer to your GoogleDrive folder. [Function that uses this scope](https://hutchdatascience.org/metricminer/reference/write_to_gsheet.html).
- `See and download all your Google Drive files.` This is used for you'd like to find and retrieve Slido data or Google Forms that are stored in your Google Drive. [Functions that use this scope](https://hutchdatascience.org/metricminer/#google-forms)
- `See information about your Google Drive files.` This is used for you'd like to retrieve Slido data or Google Forms that are stored in your Google Drive. [Functions that use this scope](https://hutchdatascience.org/metricminer/#google-forms).
- `See, edit, create, and delete only the specific Google Drive files you use with this app.` This is used for if you'd like store metrics you collect with `metricminer` to your GoogleDrive folder. [Function that uses this scope](https://hutchdatascience.org/metricminer/reference/write_to_gsheet.html).
- `View your YouTube account` This is used for collecting metrics from Youtube videos or Youtube channels with [those respective functions](https://hutchdatascience.org/metricminer/#youtube).
- `See, edit, create, and delete all your Google Sheets spreadsheets.` [Function that uses this scope](https://hutchdatascience.org/metricminer/reference/write_to_gsheet.html).
- `See and download your Google Analytics data.` This is used for collecting data from Google Analytics. [Functions that use this scope](https://hutchdatascience.org/metricminer/#google-analytics)
- `View and manage your Google Analytics data.` This is used for collecting data from Google Analytics. [Functions that use this scope](https://hutchdatascience.org/metricminer/#google-analytics).
- `See all responses to your Google Forms forms.` - This is used for you'd like to retrieve Google Form data that are stored in your Google Drive.[Functions that use this scope](https://hutchdatascience.org/metricminer/#google-forms)
- `See all your Google Forms forms.` This is used for you'd like to retrieve Google Form data that are stored in your Google Drive. [Functions that use this scope](https://hutchdatascience.org/metricminer/#google-forms)
- `See, edit, create, and delete all your Google Forms forms.` This is used for you'd like to retrieve Google Form data that are stored in your Google Drive. [Functions that use this scope](https://hutchdatascience.org/metricminer/#google-forms)


## Sharing user data
metricminer only communicates with Google APIs. No user data is shared with the owners of the metricminer, RStudio, or any other servers.

## Storing user data
These packages may store your credentials on your local machine, for later reuse by you. Use caution when using these packages on a shared machine.

By default, an OAuth token is  stored in your local environment and if when you are authorizing a package you set `cache = TRUE` your credentials will be stored in a location like, such as ~/User/<yourusername>/Library/Caches/org.R-project.R/R/metricminer/cached-secrets. But this exact location will be specific to your computer. You can retrieve the exact file path of your cached secrets by running this in R `metricminer::cached_secrets_folder()`.

## Data protection policy
We do not collect any data about you and we do not have any access to data that you retrieve by using this package. Security procedures are in place to protect the confidentiality of any that we might come across, and we use best practices, such as encryption and mandatory usage of 2FA, as a matter of course.

## Policies for authors of packages or other applications
Do not use an API key or client ID from the `metricminer` in an external package or tool. Per the Google User Data Policy https://developers.google.com/terms/api-services-user-data-policy, your application must accurately represent itself when authenticating to Google API services.

If you use these packages inside another package or application that executes its own logic — as opposed to code in the `metricminer` or by the user — you must communicate this clearly to the user. Do not use credentials from the `metricminer`; instead, use credentials associated with your project or your user.

## Contact Us  

If you have any questions about this, [you can contact us](https://www.metricminer.org/contact.html).
