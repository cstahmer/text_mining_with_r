#nstall.packages("rvest")
#install.packages("httr")

library(rvest)
library(httr)

# Appends element of a list to another without changing variable type of x
# build_url function uses the httr package and requires a variable of the url class
appendList <- function (x, val) {
  stopifnot(is.list(x), is.list(val))
  xnames <- names(x)
  for (v in names(val)) {
    x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
      appendList(x[[v]], val[[v]])
    else c(x[[v]], val[[v]])
  }
  x
}



# Simulating submit_form for GET requests
submit_geturl <- function (session, form)
{
  query <- rvest:::submit_request(form)
  query$method <- NULL
  query$encode <- NULL
  query$url <- NULL
  names(query) <- "query"
  
  relativeurl <- XML::getRelativeURL(form$url, session$url)
  basepath <- parse_url(relativeurl)
  
  fullpath <- appendList(basepath,query)
  fullpath <- build_url(fullpath)
  fullpath
}

query = "data science"
loc = "New York"
session <- html_session("http://www.indeed.com")
form <- html_form(session)[[1]]
form <- set_values(form, q = query, l = loc)



# Submit form and get new url
session1 <- submit_form2(session, form)

# Get reviews of last company using follow_link()
session2 <- follow_link(session1, css = "#more_9 li:nth-child(3) a")
reviews <- session2 %>% html_nodes(".description") %>% html_text()
reviews



