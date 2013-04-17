\name{bef.tematres.search}
\alias{bef.search}
\alias{bef.tematres.search}
\title{A wrapper for the search on the tematres thesaurus.}
\usage{
  bef.tematres.search(lookup_keyword,
    search_task = "search")
}
\arguments{
  \item{lookup_keyword}{The keyword you are looking for}

  \item{search_task}{Defines behavior on search. Defaults
  to "search" but can also be "broader" (look for upward
  definitions) and "narrower" (looking for downward
  definitions).}
}
\value{
  The function returns a vector of keywords
}
\description{
  Search a Tematres thesaurus via keywords. This function
  is a wrapper and so it calls the appropriate funtions
  depending on the search task.
}

