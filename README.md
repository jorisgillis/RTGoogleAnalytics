
## What is it

RTGoogleAnalytics is a R Wrapper around the [Google 
Analytics](http://www.google.com/analytics/) Real Time API. It allows fast and 
easy data extraction in R. This project is a fork of the [RGoogleAnalytics 
package](https://github.com/Tatvic/RGoogleAnalytics) by Vignesh Prajapati 
(Tatvic).

## Key Features

* Provides Access to v3 of the [Google Analytics Real Time Reporting 
API](https://developers.google.com/analytics/devguides/reporting/realtime/v3/)

* Supports authorization via OAuth 2.0

## Installation

To get the current development version from github:

```R # require(devtools) 
devtools::install_github("jorisgillis/RTGoogleAnalytics") ```

## Dependencies

* [httr](http://cran.r-project.org/web/packages/httr/index.html) handles the 
underlying OAuth2.0 Authorization flow and the API requests

* [lubridate](http://cran.r-project.org/web/packages/lubridate/index.html) 
handles the date manipulation logic underlying Query Partitioning

## Background

Work on RGoogleAnalytics was started by Michael Pearmain at Google. He was 
supported by Nick Mihailowski (Google) and Vignesh Prajapati (Tatvic).

## Tutorials and Use-cases

Under development

## Important Links

* 
[List](https://developers.google.com/analytics/devguides/reporting/realtime/dimsmets)
of Valid Dimension/Metric Combinations from the Google Analytics Real Time API
Reference Guide

* [Query Feed Explorer](http://ga-dev-tools.appspot.com/explorer/) allows you to
test your queries for syntatical correctness. Once verified, the query 
parameters can then be copied to your R Script