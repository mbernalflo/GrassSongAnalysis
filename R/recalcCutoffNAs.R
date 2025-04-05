#' Recalculated stroph durations and other NAs from cutoffs.
#' 
#' Takes the cutoff points and recalcuateds begin, end and duration in seconds. Also 
#' re-numbers strophs.
#'
#' @param stroCutoffs A \code{data.frame} containing the strophe cutoff locations 
#'   as returned by \code{\link{identifyStrophes}}.
#' @param f Sampling rate (extracted from wavfile if NULL).
#' @param wl Window length for the (finest) sound envelope given as the 
#'   number of sample points, At f=44100: 44 eq 1 ms (recommended), 441 eq 10 
#'   ms, 4410 eq 100 ms.
#' @param ovlp overlap between two successive windows (in percent, i.e., 0.
#' @param padding Boolean or number. If FALSE or 0, no padding will be applied. 
#'   If a number >0, padding at the beginning and end will be applied with 
#'   padding interpreted as the number of samples. If TRUE, padding at the 
#'   beginning and end will be applied with the number of samples determined 
#'   by the \code{StroThreshExceedWW - StroThreshNeeded + 1} (recommended).
#' @param file Name of file, to which results should be written. No file is written
#'   if \code{file = NA} (the default).
#' 
#' @return 
#' Returns a \code{data.frame}, which is \code{Cutoffs} in which NAs are updated.
#' 
#' @author Holger Schielzeth  (holger.schielzeth@@uni-bielefeld.de).
#' 
#' @seealso 
#' \code{\link{loadSongfile}}, \code{\link{curateStrophes}}, \code{\link{identifyStrophes}}.
#' 
#' @examples  
#' stroCutoffs <- data.frame(
#'   StartSampWithPad = c(1000, 2000, 3000),
#'   EndSampWithPad = c(1500, 2500, 3500)) 
#' recalcCutoffNAs(stroCutoffs,f=4410,wl=44,ovlp=0,padding=FALSE, file = NA)
#' @export


# Helper function to recalculate NAs in Cutoffs file
recalcCutoffNAs = function(stroCutoffs, f, wl, ovlp, padding, file=NA) {
  stroCutoffs$StroCounter      = 1:nrow(stroCutoffs)
  stroCutoffs$StartSampWoutPad = stroCutoffs$StartSampWithPad - padding
  stroCutoffs$EndSampWoutPad   = stroCutoffs$EndSampWithPad - padding
  stroCutoffs$StartSec         = stroCutoffs$StartSampWoutPad/(f/(wl*(1-ovlp/100)))
  stroCutoffs$EndSec           = stroCutoffs$EndSampWoutPad/(f/(wl*(1-ovlp/100)))
  stroCutoffs$DurSec           = stroCutoffs$EndSec - stroCutoffs$StartSec
  #stroCutoffs$StroValid        = as.numeric(stroCutoffs$DurSec > MinStroDur)
  if(!is.na(file)) write.table(stroCutoffs, file=file, sep="\t", quote=FALSE, row.names=FALSE)
  return(stroCutoffs)
}
