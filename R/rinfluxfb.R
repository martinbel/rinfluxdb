# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


#' @export
#' @param response (httr::GET output)
#' @param return_datatable (bool)
#' @return a data.frame or data.table
parse_response <- function(response, return_datatable=TRUE){
  res = content(response)[[1]][[1]][[2]]
  cols = unlist(res[[1]]$columns)
  dt = rbindlist(res[[1]]$values)
  setnames(dt, names(dt), cols)
  if(return_datatable){
    return(dt)
  } else {
    return(as.data.frame(dt))
  }
}


#' @export
#' @param con (list) Should include scheme, host, port
#' @param args (list) HTTP arguments in list format, at least include db, query
#' @param return_datatable (boolean) How should the data be returned, by default it returns a data.table
#' @return data.table or data.frame from query
dbGetQuery <- function(con,
                       args,
                       return_datatable=TRUE
                       ) {


  make_url = sprintf("%s://%s:%s/query?",
                     scheme=con[['scheme']], host=con[['host']], port=con[['port']])

  response <- GET(
    make_url,
    query = args
  )

  if (response$status_code < 200 || response$status_code >= 300) {
    if (length(response$content) > 0)
      warning(rawToChar(response$content))
    stop("Influx query failed with HTTP status code ", response$status_code)
  } else {
    DT = parse_response(response, return_datatable=return_datatable)
    if(is.data.frame(DT)){
      return(DT)
    } else {
      return(response)
    }
  }

}
