\name{FourScores} 
\alias{FourScores}
\title{
  FourScores - a two player game
}
\description{
  A game for two player: Who gets first four in a row wins. Published by Milton Bradley, designed by Howard Wexler and Ned Strongin.
}
\usage{
  FourScores(modus = 'normal', rows = 6, collumns = 7)
}

\arguments{
  \item{modus}{
    modus 'normal' means that the players are asked to type a name. If not wanted use 'quick' instead .
  }
  \item{rows}{
    number of rows for the playing field
  }
  \item{collumns}{
    number of collumns for the playing field
  }
}

\references{
  Published by Milton Bradley, designed by Howard Wexler and Ned Strongin.
}
\author{
  Matthias Speidel
}

\seealso{
  \code{\link{getGraphicsEvent}}
  http://en.wikipedia.org/wiki/Connect_Four
}
