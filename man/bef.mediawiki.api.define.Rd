\name{bef.mediawiki.api.define}
\alias{bef.mediawiki.api.define}
\title{Basic Mediawik API with one task to retrieve descriptions}
\usage{
  bef.mediawiki.api.define(term, language = "en")
}
\arguments{
  \item{term}{Is the term or string you are searching for
  on Wikipedia.}

  \item{language}{Is the language of the Wikipedia
  (default: en). You can use two character country codes
  that Wikipedia uses in their sub domains.}
}
\value{
  The function returns a vector with description strings.
}
\description{
  This function queries Wikipedia in the language of your
  choice. You can select the language per two character
  country code Wikipedia uses in they sub domains.
}
\examples{
\dontrun{
		descriptions = bef.mediawiki.api.define(term = "Tree")
		descriptions = bef.mediawiki.api.define(term = "Baum", language = "de")
	     }
}

