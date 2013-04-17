\name{bef.tematres.api}
\alias{bef.tematres.api}
\title{Basic tematres server api}
\usage{
  bef.tematres.api(task = "availableTasks", argument)
}
\arguments{
  \item{task}{The api task you like to execute. Use the the
  "availableTasks" to get an overview about the base api.
  It returns a data frame with descriptions and the
  arguments for the tasks.}

  \item{argument}{Is the argument for the api task. You
  find the information about the arguments when you call
  the task "availableTasks".}
}
\value{
  The function returns either a dataframe for information
  or a list of keywords and ids
}
\description{
  Basic tematres server api
}
\examples{
\dontrun{
	 bef.tematres.api(task = "availableTasks")
	 bef.tematres.api(task = "fetchVocabularyData")
	 bef.tematres.api(task = "fetchTopTerms")
	 bef.tematres.api(task = "search", argument = "measurement")
	 bef.tematres.api(task = "letter", argument = "t")
	 bef.tematres.api(task = "fetchTerm", argument = 12)
	 bef.tematres.api(task = "fetchDown", argument = 4 )
	 bef.tematres.api(task = "fetchUp", argument = 4)
	 bef.tematres.api(task = "fetchRelated", argument = 4)
	 bef.tematres.api(task = "fetchAlt", argument = 12 )
	 bef.tematres.api(task = "fetchCode", argument = "tree")
	 bef.tematres.api(task = "fetchNotes", argument = 5 )
	 bef.tematres.api(task = "fetchDirectTerms", argument = 12)
	 bef.tematres.api(task = "fetchURI", argument = 12)
	 bef.tematres.api(task = "fetchTargetTerms", argument = 12 )
	 bef.tematres.api(task = "fetchSourceTerm", argument = 12)
	 bef.tematres.api(task = "fetchTerms", argument = '12,13' )
	 bef.tematres.api(task = "fetchRelatedTerms", argument = '12,13')
	 bef.tematres.api(task = "fetchSimilar", argument = 12)
	 bef.tematres.api(task = "fetchLast")
	 bef.tematres.api.conversion.keyword_id(given = "Measurement")
	 bef.tematres.api.conversion.keyword_id(given = 8)
 }
}

