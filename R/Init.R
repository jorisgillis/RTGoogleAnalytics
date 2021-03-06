#' Initialize the Google Analytics query parameters
#' 
#' This function takes all the query parameters and combines them into a single list that 
#' is to be passed as an argument to \code{\link{QueryBuilder}}. Note that parameter validation is 
#' performed when the \code{\link{QueryBuilder}} object is created. 
#'
#' @export
#' @param dimensions Optional. A vector of up to 7 dimensions, either as a single string or a vector or strings, E.g.
#'                  "ga:source,ga:medium" or c("ga:source", "ga:medium").      
#'                      
#' @param metrics A vector of up to 10 metrics, either as a single string or a vector or strings. E.g.
#'                "ga:sessions" or c("ga:sessions", "ga:bounces").  
#' 
#' @param sort Optional.The sorting order for the data to be returned.e.g. "ga:sessions" or c("ga:sessions", "-ga:browser") 
#' 
#' @param filters Optional.The filter string for the GA request.e.g. "ga:medium==referral".
#' 
#' @param max.results Optional.Maximum Number of rows to include in the query response. Default value is 
#' 10000
#' 
#' @param table.id Profile ID of the form ga:XXXXX where XXXXX is the Analytics View (Profile) ID of 
#' for which the query will retrieve the data. The View ID can be found under View Settings by navigating 
#' to the Admin Tab under your Google Analytics Profile
#'  
#' @seealso
#' Valid Combinations of Dimensions and Metrics can be found at  \url{https://developers.google.com/analytics/devguides/reporting/realtime/v3/devguide#constraints}
#' 
#' 
#' @return List of all the Query Parameters initialized by the user

Init <- function(
  dimensions = NULL,
  metrics = NULL,
  filters = NULL,
  sort = NULL,
  max.results = NULL,
  table.id = NULL) {
  
  query.params.list = list("dimensions" = dimensions,
                           "metrics" = metrics,
                           "filters" = filters,
                           "sort" = sort,
                           "max.results" = max.results,
                           "table.id" = table.id)
  
  return(query.params.list)
}
