#' 任意の Metric 値を Azure Monitor API 経由にて取得
#'
#' @param SUBSCRIPTION_ID character
#' @param RESOURCE_GROUP_NAME character
#' @param RESOURCE_PROVIDER_NAMESPACE character
#' @param RESOURCE_TYPE character
#' @param RESOURCE_NAME character
#' @param AZUREAD_TOKEN character
#' @param AGGREGATION_TYPE character
#' @param METRIC character
#' @param TIMEGRAIN character
#' @param STARTTIME character
#' @param ENDTIME character
#'
#' @return result data.frame Metric の値
#'
#' @export
getMonitorMetricValue <- function(SUBSCRIPTION_ID, RESOURCE_GROUP_NAME, RESOURCE_PROVIDER_NAMESPACE
                                 ,RESOURCE_TYPE, RESOURCE_NAME, AZUREAD_TOKEN
                                 ,AGGREGATION_TYPE, METRIC, TIMEGRAIN
                                 ,STARTTIME, ENDTIME){
  API_VERSION <- "2016-09-01"

  FILTER <- paste0( "(name.value eq '" , METRIC , "') and aggregationType eq '" ,AGGREGATION_TYPE
                    ,"' and startTime eq " ,STARTTIME
                    ," and endTime eq " ,ENDTIME
                    ," and timeGrain eq duration'" ,TIMEGRAIN , "'"
                    ) %>%
    URLencode

  URL <- paste0("https://management.azure.com/subscriptions/" ,SUBSCRIPTION_ID
                ,"/resourceGroups/" ,RESOURCE_GROUP_NAME
                ,"/providers/" ,RESOURCE_PROVIDER_NAMESPACE, "/" ,RESOURCE_TYPE ,"/" ,RESOURCE_NAME
                ,"/providers/microsoft.insights/metrics?$filter=" ,FILTER
                ,"&api-version=" , API_VERSION
                )

  HEADER <- c('Authorization' = paste("Bearer", AZUREAD_TOKEN, sep = " ")
              ,'Accept' = "application/json"
  )

  result <- RCurl::getURL(URL, .opts = list(httpheader = HEADER), .encoding = 'UTF-8') %>%
    jsonlite::fromJSON(., flatten = TRUE) %>%
    magrittr::extract2(1) %>% select(data) %>% magrittr::extract2(1) %>% magrittr::extract2(1)

  # for debug
  # result <- RCurl::getURL(URL, .opts = list(httpheader = HEADER), .encoding = 'UTF-8') %>%
  #    jsonlite::fromJSON(., flatten = TRUE)

  # 日時を JST に変換
  format <- "%Y-%m-%dT%H:%M:%S"
  cha <- as.POSIXct(result$timeStamp, format = format) %>%
    as.character(.)
  result$timeStamp <- as.POSIXlt(as.POSIXct(cha, tz="UTC"),tz="Japan")

  return(result)
}
