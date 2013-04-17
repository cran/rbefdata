\name{bef.portal.vizualize.keywords}
\alias{bef.portal.vizualize.keywords}
\alias{bef.vizualize.keywords}
\title{Vizualization functions to your BEFdata portal}
\usage{
  bef.portal.vizualize.keywords(color_pal = brewer.pal(8, "Dark2"),
    min_freq = 1)
}
\arguments{
  \item{color_pal}{Is a color palette you can create for
  example with the color brewer package. See in the
  examples below. This defaults to brewer.pal(8,"Dark2").}

  \item{min_freq}{I passed to the wordcloud function and
  delimits the minimum freqency the keyword needs to appear
  in the cloud. This defaults to 1.}
}
\value{
  The function returns a wordcloud based on the keywords of
  the BEFdata portal.
}
\description{
  Vizalize the keywords contained in your BEFdata portal
  vai a nice wordcloud.
}
\examples{
\dontrun{
	       require(RColorBrewer)
            my_palette = brewer.pal(8,"Dark2")
	       bef.portal.vizualize.keywords()
          }
}

