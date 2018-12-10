getCase <- function(searchTerm, from, to) {
  start <- as.Date(paste0(from, "-01-01"),format="%Y-%m-%d")
  end <- as.Date(paste0(to, "-12-31"), format="%Y-%m-%d")
  query <- paste0(URL, 
                  URLencode(searchTerm), 
                  "&decision_date_min=", 
                  format(start,"%Y-%m-%d"), 
                  "&decision_date_max=", 
                  format(end,"%Y-%m-%d"))
  getdata <- GET(url = query, add_headers(Authorization=API_KEY))
  return(fromJSON(content(getdata, type='text'))$results[[1]])
}

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}