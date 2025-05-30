% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measureSyllables.R
\name{measureSyllables}
\alias{measureSyllables}
\title{Extract measurements for syllables or pulses.}
\usage{
measureSyllables(
  song,
  envFine,
  wavfile,
  syllpath,
  syllCutoffs = NULL,
  useResFile = TRUE
)
}
\arguments{
\item{envFine}{A sound envelope as a vector of numbers.}

\item{wavfile}{Name of wavfile (necessary).}

\item{syllpath}{Path to folder in wich bmp files should be plotted 
(optional, working directory if missing).}

\item{syllCutoffs}{TO BE ADDED}

\item{useResFile}{Boolean. A check is done, if a results file already exists and
the results of this file are used in \code{useResFile = TRUE}}
}
\value{
Returns a \code{data.frame}, which is \code{Cutoffs} augmented with five 
columns of measurements.
}
\description{
Exacts the start, end, max, min and mean amplitute for each syllable/pulse.
}
\examples{
  

\dontrun{

mysound = filePreparation("myfile.wav", "C://mypath//", 
myenv = plotEnvolopes(mysound, "myfile.wav", "c://mystropath//",
  padding = 300)
mystro = plotStrophes(myenv$envFine, "myfile.wav", "C://mypath//", 
  measure=TRUE)
}

}
\references{
No references.
}
\seealso{
\code{\link{loadSongfile}}, \code{\link{curateStrophes}}, 
\code{\link{identifyStrophes}}, \code{\link{identifySyllables}}, 
\code{\link{measureSyllables}}, \code{\link{curateSyllables}}
}
\author{
Holger Schielzeth  (holger.schielzeth@uni-bielefeld.de).
}
