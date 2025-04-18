% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identifyStrophes.R
\name{identifyStrophes}
\alias{identifyStrophes}
\title{Identify and plot sound envelopes and identify strophe cutoffs.}
\usage{
identifyStrophes(
  envFine,
  wavfile,
  stropath = "",
  f = 44100,
  plot = TRUE,
  toFile = TRUE,
  bmpwidth = 3200,
  bmpheight = 600,
  rollmeanStroWW = c(11, 101),
  wl = 44,
  ovlp = 50,
  padding = TRUE,
  stroAmpThresh = 15,
  StroThreshExceedWW = 301,
  StroThreshNeeded = 10,
  MinStroDur = 0.5,
  stroCutoffs = NULL,
  writeRes = TRUE,
  overwriteRes = FALSE,
  useResFile = TRUE,
  xlim = NA
)
}
\arguments{
\item{envFine}{A sound envelope as a vector of numbers.}

\item{wavfile}{Name of wavfile (required).}

\item{stropath}{Path to folder in which jpg files should be plotted 
(optional, working directory if missing). Directory will be created if 
it does not exist.}

\item{f}{Sampling rate (extracted from wavfile if NULL).}

\item{plot}{Boolean, whether or not a plot should be in created.}

\item{toFile}{Boolean, whether or not the plot should be in a .jpg file 
(if TRUE) or in a plotting window (if FALSE).}

\item{bmpwidth}{Width of the jpg image (in pixels).}

\item{bmpheight}{Height of the jpg image (in pixels).}

\item{rollmeanStroWW}{Vector of 2 (preferentially odd) numbers. The 
rolling mean window width for determining the intermediate an coarse 
envolopes from the finest resultion envolope determined by \code{wl} and 
\code{ovlp}. The code works more accurate for odd numbers!}

\item{wl}{Window length for the (finest) sound envelope given as the 
number of sample points, At f=44100: 44 eq 1 ms (recommended), 441 eq 10 
ms, 4410 eq 100 ms.}

\item{ovlp}{overlap between two successive windows (in percent, i.e., 0.}

\item{padding}{Boolean or number. If FALSE or 0, no padding will be applied. 
If a number >0, padding at the beginning and end will be applied with 
padding interpreted as the number of samples. If TRUE, padding at the 
beginning and end will be applied with the number of samples determined 
by the \code{StroThreshExceedWW - StroThreshNeeded + 1} (recommended).}

\item{stroAmpThresh}{Envelope threshold for identification of silence/sound 
(i.e., Strophes). Recommended value depends on background noise level.}

\item{StroThreshExceedWW}{The number of samples that is inspected for the 
identification of strophe beginning and ends.}

\item{StroThreshNeeded}{The number of samples within the window determined 
by \code{StroThreshExceedWW} that have to exceed the threshold determined 
by \code{stroAmpThresh} in order to qualify as a sound.}

\item{MinStroDur}{A minimum duration for potential strophes to be analyzed 
(in seconds).}

\item{stroCutoffs}{Optional \code{data.frame} containing strophe 
cutoffpoints (as returned by \code{stroIdent}). If provided, strophe 
cutoff identification will be skipped and strophe cutoffs will be plotted 
as provided. Note that the padding has to be the same in the 
\code{stroCutoffs} as in \code{padding}, since cutoffs will be plotted 
with padding!).}

\item{writeRes}{Boolean. Whether or not the strophe cutoffs should be 
written to file.}

\item{overwriteRes}{Boolean. This is just a safty catch. If  \code{write = TRUE}
and \code{overwriteRes = FALSE} the function will stop in order not to 
overwrite previous manual edits.}

\item{useResFile}{Boolean. If \code{useResFile = TRUE} a check is done to see if there are previous files that can be used as results. If there are previous files but are not to be used, set \code{useResFile = FALSE} and consider also setting \code{overwriteRes = TRUE}.}

\item{xlim}{Vector of two numbers used to set x-limits in plot. Useful only to 
zoom in when cutoffs are know.}
}
\value{
Returns the a list of 2 Elements:
\describe{
\item{stroCutoffs}{A \code{data.frame} of detected strophe cutoff points. 
  The elements are:
  \describe{
     \item{StartSampWithPad}{The strophe start as the sample in 
       \code{envFine} in the padded sequence.}
     \item{EndSampWithPad}{The strophe end as the sample in \code{envFine} 
       in the padded sequence.}
     \item{StartSampWoutPad}{The strophe start as the sample in 
       \code{envFine} in sequence without padding.}
     \item{EndSampWoutPad}{The strophe end as the sample in \code{envFine} 
       in sequence without padding.}
     \item{StartSec}{The strophe start converted to seconds of the song 
       without padding.}
     \item{EndSec}{The strophe end converted to seconds of the song 
       without padding.}
     \item{DurSec}{The strophe duration in seconds.}
     \item{StroCounter}{A counter for strophes within the \code{song}.}
     \item{StroValid}{Idicator if the strophe is valid (i.e., exceeds 
       \code{MinStroDur}).}
   }
  }
}
}
\description{
Idendifies sound envelopes at two levels (on derived from the 
\code{\link{Wave}} object and two determined by smoothing. Plots the sound 
envelopes at the three levels in one plot, the difference between the 
coarsest and intermediate level in a second plot (for syllable 
identification) and the differeence between finest and intermidiate level 
in a third plot (in a third plot). Also applies automated strophe 
identification.
}
\details{
The algorithm in brief:
\itemize{
 \item Determine sound envelope with parameters \code{wl} and \code{ovlp} 
   using the function \code{\link{env}} from the \code{link{seewave}} 
   package (using the default \code{envt = "hil"}.
 \item Use a sliding window to determine the smoothed envelopes.
 \item Extend the sliding window sequences at the ends (by simply 
   replicating the first and last elements) so that the smoothed envelopes 
   have the same length as the original.
 \item Search for windows of size \code{StroThreshExceedWW} in the 
   coaresets resolution that do/do no exceed the amplitude threshold 
   determined by \code{stroAmpThresh}.
 \item Do some plotting and packaging of the results.
}
}
\note{
Contains two work-arounds to avoid cutoffs that run in a sequence and uneven 
numbers of cutoffs (since usually start and end cutoffs are expected to occur 
in pairs).
}
\examples{
  

\dontrun{

mysound = filePreparation("myfile.wav", "C://mypath//", highpassfilter=1001, select=c(0.5,2))
plotEnvolopes(mysound, "myfile.wav", "c://mystropath//", padding = 300)
}

}
\seealso{
\code{\link{loadSongfile}}, \code{\link{curateStrophes}}, 
\code{\link{identifySyllables}}, \code{\link{measureSyllables}}, 
\code{\link{identifyPulses}}, \code{\link{curateSyllables}}
}
\author{
Holger Schielzeth  (holger.schielzeth@uni-jena.de)
}
