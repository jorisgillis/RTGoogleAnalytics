
# Recheck this test later on
# test_that("Query builder initializes a list", {
# 	test.list <- list(start.date  = function(start.date = NULL){},
#                     end.date    = function(end.date = NULL) {},
#                     dimensions  = function(dimensions = NULL) {},
#                     metrics     = function(metrics = NULL) {},
#                     segment     = function(segment = NULL) {},
#                     sort        = function(sort = NULL) {},
#                     filters     = function(filters = NULL) {},
#                     max.results = function(max.results = NULL) {},
#                     start.index = function(start.index = NULL) {},
#                     table.id    = function(table.id = NULL) {},
#                     build       = function(build = NULL) {})
# 	test.QueryBuilder <- QueryBuilder()
#   # Test the structure of the object returned by QueryBuilder() match.
#   expect_that(str(test.list), equals(str((test.QueryBuilder))))
# }) 


##-----------------------------------------------------------------------------
## Setting up tests
##-----------------------------------------------------------------------------
# List of query parameters
query.list <- Init(dimensions = "ga:date,ga:pagePath,ga:hour",
                   metrics = "ga:sessions,ga:pageviews",
                   sort = "-ga:sessions",
                   max.results = 1234,
                   table.id = "ga:33093633")

# Create the Query Builder object so that the query parameters are validated
query <- QueryBuilder(query.list)

##-----------------------------------------------------------------------------
context("Dimensions")

test_that("Passing a string as a parameter works", {
    query$dimensions("ga:source,ga:medium")
    expect_that("ga:source,ga:medium",equals(query$dimensions()))
})

test_that("Value of Dimensions is unset if input is NULL", {
    query$dimensions(NULL)
    expect_that(NULL, equals(query$dimensions()), info= "Passed for dimension check")
})

test_that("Non vector values are not accepted as dimensions", {
    dimensions.not.vector <- as.Date("1981-06-26", "%Y-%m-%d")
    expect_that(query$dimensions(dimensions.not.vector),throws_error())  
})

test_that("Length of Dimension vector does not exceed 7 dimensions", {
    dimensions.high <- c("ga:source",
                       "ga:medium",
                       "ga:source",
                       "ga:medium",
                       "ga:source",
                       "ga:medium",
                       "ga:source",
                       "ga:medium")
    expect_that(query$dimensions(dimensions.high),throws_error())                     
})

test_that("Only Character Vectors are accepted as dimensions", {
    dimensions.numeric <- c(1, 2, 3, 4, 5)
    expect_that(query$dimensions(dimensions.numeric),throws_error())  
})

context("Metrics")

test_that("Passing a string as a parameter works", {
    query$metrics("ga:pageviews,ga:sessions")
    expect_that("ga:pageviews,ga:sessions",equals(query$metrics()))
})

test_that("Passing a vector of metrics works", {
    query$metrics(c("ga:transactions","ga:transactionsPerSession"))
    expect_that("ga:transactions,ga:transactionsPerSession",equals(query$metrics()))
})

test_that("Value of metrics is unset if input is NULL", {
    query$metrics(NULL)
    expect_that(NULL, equals(query$metrics()), info= "Passed for metrics check")
})

test_that("Non vector values are not accepted as metrics", {
    metrics.not.vector <- as.Date("1981-06-26", "%Y-%m-%d")
    expect_that(query$metrics(metrics.not.vector),throws_error())  
})

test_that("Length of Dimension vector does not exceed 7 dimensions",{
    metrics.high <- c("ga:pageviews", "ga:sessions",
                    "ga:pageviews", "ga:sessions",
                    "ga:pageviews", "ga:sessions",
                    "ga:pageviews", "ga:sessions",
                    "ga:pageviews", "ga:sessions",
                    "ga:pageviews")  
    expect_that(query$metrics(metrics.high),throws_error())
})

test_that("Only Character Vectors are accepted as metrics", {
    metrics.numeric <- c(1, 2, 3, 4, 5)
    expect_that(query$metrics(metrics.numeric),throws_error())  
})

context("Sort")

test_that("Passing string as a parameter to sort works", {
    query$sort("ga:source,ga:medium")
    expect_that("ga:source,ga:medium",equals(query$sort()))
})

test_that("Passing a vector as a parameter works", {
    query$sort(c("ga:pageviews","ga:sessions"))
    expect_that("ga:pageviews,ga:sessions",equals(query$sort()))
})

test_that("Passing a NULL value unsets the parameter",{
    query$sort(NULL)
    expect_that(NULL,equals(query$sort()))
})

test_that("Non vector values raise an error", {
    sort.not.vector <- as.Date("2014-11-01", "%Y-%m-%d")
    expect_that(query$sort(sort.not.vector),throws_error())
})

test_that("Non Character vector raises an error", {
    sort.numeric <- c(1,2,3,4,5)
    expect_that(query$sort(sort.numeric),throws_error())
}) 

# Recheck these
context("Filters")

test_that("Passing a valid string as a parameter works", {
    filter <- "ga:medium==referral,ga:source==google"
    query$filters(filter)
    expect_that(filter,equals(query$filters()))
})

test_that("Passing a NULL value unsets the parameter", {
    query$filters(NULL)
    expect_that(NULL,equals(query$filters()))
})

context("Max Results")

test_that("Passing a valid number as as parameter works", {
    query$max.results(5000)
    expect_that(5000,equals(query$max.results()))
}) 

test_that("Passing a NULL value unsets the parameter", {
    query$max.results(NULL)
    expect_that(NULL,equals(query$max.results()))
})

test_that("Passing a vector of values throws an error", {
    expect_that(query$max.results(c(1,2,3)),throws_error())
})

test_that("Passing a string of values throws an error", {
    expect_that(query$max.results("5000"), throws_error())
})

context("Table ID")

test_that("Passing a valid string as a parameter works", {
    query$table.id("ga:1174")
    expect_that("ga:1174",equals(query$table.id()))
})

test_that("Passing a NULL value unsets the parameter", {
    query$table.id(NULL)
    expect_that(NULL,equals(query$table.id()))
})

test_that("Passing a vector raises an error", {
    expect_that(query$table.id(c("ga:1234","ga:4567")),throws_error())
})

test_that("Passing non vector values raises an error", {
    table.id.not.vector <- as.Date("2014-11-01", "%Y-%m-%d")
    expect_that(query$table.id(table.id.not.vector),throws_error())
})

test_that("Passing numeric values raises an error", {
    expect_that(query$table.id(1175),throws_error())
})

context("To URI")

# This test requires the RCurl package to be loaded in the namespace
# How to handle this
# This test assumes that the parameters pass the parameters test
test_that("To URI function is working appropriately", {
  expected.uri <- paste("https://www.googleapis.com/analytics/v3/data/realtime",
                        "?dimensions=ga%3adate",
                        "&metrics=ga%3asessions",
                        "&sort=ga%3adate",
                        "&filters=ga%3asourceMedium%3d~%5egoogle%20%2f%20organic%24%7c%5egoogle%20%2f%20cpc%24",
                        "&max-results=10000",
                        "&ids=ga%3a30661272",
                        sep = "")
  query.params.list = list("dimensions" = "ga:date",
                           "metrics" = "ga:sessions",
                           "sort" = "ga:date",
                           "filters"="ga:sourceMedium=~^google / organic$|^google / cpc$",
                           "max.results" = 10000,
                           "table.id" = "ga:30661272")
  query.builder = QueryBuilder(query.params.list)
  token <- list(credentials=list(access_token=NULL))

  expect_that(expected.uri,equals(ToUri(query.builder, token)))
})

# test_that("Setting all query parameters as NULL works", {
# 
#   query$SetStartDate(NULL)
#   query$SetEndDate(NULL)
#   query$dimensions(NULL)
#   query$metrics(NULL)
#   query$segment(NULL)
#   query$filters(NULL)
#   query$sort(NULL)
#   query$max.results(NULL)
#   query$SetStartIndex(NULL)
#   query$table.id(NULL)
# 
#   expect_that("https://www.googleapis.com/analytics/v3/data/ga?",equals(query$to.uri()))  
# })

context("Validate")

builder <- QueryBuilder(query.list)

test_that("Missing metrics results in an error", {
  builder$table.id("ga:30661272")
  expect_that(builder$validate(),throws_error())
})

test_that("Missing table ID results in an error", {
  builder$metrics("ga:users")
  expect_that(builder$validate(),throws_error())
})
