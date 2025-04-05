#' Function for automatically fixing detected strophe cutoffs.
#'
#' Temp.... 
#'
#' @param envFine Sound envelope (typically at finest resolution, required).
#' @param wavfile Name of wavfile (required).
#' @param stropath Path of the strophe files (optional, working directory if 
#'   missing).
#' @param f Sampling rate of .wav file. Requrired for conversion of sampling 
#'   units to seconds.
#' @param wl Window width used when calculating the sound evolope. Requrired 
#'   for conversion of sampling units to seconds.
#' @param ovlp Overlap of the windows (expressed as percentages, i.e. >=0 and 
#'   <100) used when calculating the sound evolope. Requrired for conversion 
#'   of sampling units to seconds.
#' @param StroThreshExceedWW The number of samples that is inspected for the 
#'   identification of strophe beginning and ends.
#' @param StroThreshNeeded The number of samples within the window determined 
#'   by \code{StroThreshExceedWW} that have to exceed the threshold determined 
#'   by \code{stroAmpThresh} in order to qualify as a sound.
#' @param padding Boolean or number. If FALSE or 0, no padding will be applied. 
#'   If a number >0, padding at the beginning and end will be applied with 
#'   padding interpreted as the number of samples. If TRUE, padding at the 
#'   beginning and end will be applied with the number of samples determined by 
#'   the \code{StroThreshExceedWW - StroThreshNeeded + 1} (recommended).
#' @param rollmeanStroWW Vector of 2 (preferentially odd) numbers. The 
#'   rolling mean window width for determining the intermediate an coarse 
#'   envolopes from the finest resultion envolope determined by \code{wl} and 
#'   \code{ovlp}. The code works more accurate for odd numbers!
#' @param MinStroDur A minimum duration for potential strophes to be analyzed 
#'   (in seconds).
#' @param stroCutoffs (Optional) \code{data.frame} with strophe cutoffs.
#' @param confirmRevised Boolean. Whether or not the revised plot should be 
#'   shown on screen.
#' @param replotToFile Boolean. Whether or not the plot should be written to file.
#' @param bmpwidth Width of the jpg image (in pixels). Applies only to plot 
#'   written to file.
#' @param bmpheight Height of the jpg image (in pixels).
#' @param closeAllWindowsOnStart Boolean. Whether or not plotting windows should be
#'   closed at the start.
#' @param manualEdit Boolean. Whether or not manual edits should be applied. Otherwise
#'   NAs in StroCutoffs are simply updated (and Stroph numbers reordered!).
#' @param forceStartAtZero Boolean. Whether predictated starting times in the padding 
#'   area should be forced to start at point zero of the actual recording. 
#' @param ... Addional parameters handed over to \code{\link{identifyStrophes}} 
#'   for plotting.
#' 
#' @return 
#' Returns a \code{data.frame} of strophe cutoffs 
#' (\code{\link{identifyStrophes}} for details).
#' 
#' @author Holger Schielzeth  (holger.schielzeth@@uni-bielefeld.de)
#' 
#' @seealso 
#' \code{\link{loadSongfile}}, \code{\link{identifyStrophes}}, 
#' \code{\link{identifySyllables}}, \code{\link{measureSyllables}}, 
#' \code{\link{identifyPulses}}, \code{\link{curateSyllables}}
#' 
#' @examples   
#' \dontrun{
#' # No example yet.
#' }
#' 
#' @export

fixStrophesPadding = function(envFine, wavfile, stropath, stroCutoffs=NULL, f=44100, wl=44, ovlp=50, padding=TRUE, 
                          rollmeanStroWW=c(11, 101), StroThreshExceedWW=0, StroThreshNeeded=0, stroAmpThresh=15, MinStroDur=0, 
                          confirmRevised = TRUE, replotToFile = TRUE, closeAllWindowsOnStart=TRUE, manualEdit = TRUE, 
                          bmpwidth=3200, bmpheight=600, forceStartAtZero = TRUE, ...) {
  # Initial checks
  if(is.logical(padding) && padding) padding = StroThreshExceedWW - StroThreshNeeded + 1
  if(is.logical(padding) && padding ==FALSE || is.null(padding)) padding = 0
  if(closeAllWindowsOnStart) replicate(length(dev.list()), dev.off())
  
  if(forceStartAtZero && any(stroCutoffs$StartSampWoutPad<0))  {
    stroCutoffs$StartSampWithPad[stroCutoffs$StartSampWithPad<padding] = padding
    #stroCutoffs$StroCounter      = 1:nrow(stroCutoffs)
    stroCutoffs[, 4:8] = NA
    stroCutoffs = recalcCutoffNAs(stroCutoffs, f = f, wl = wl, ovlp = ovlp, padding = padding)
  }
  write.table(stroCutoffs, file=paste0(stropath, unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],".txt"), sep="\t", quote=FALSE, row.names=FALSE)
  #stroCutoffs = read.table(paste0(stropath, unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],".txt"), header=TRUE, sep="\t")
  #if(any(is.na(stroCutoffs))) 
  #  stroCutoffs = recalcCutoffNAs(stroCutoffs,f=f, wl=wl, ovlp=ovlp, padding=padding, file=paste0(stropath, unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],".txt"))
  if(replotToFile) {
    identifyStrophes(envFine, wavfile=wavfile, stropath=stropath, toFile=TRUE, 
                     wl=wl, ovlp=ovlp, padding=padding, MinStroDur=MinStroDur,
                     StroThreshExceedWW=StroThreshExceedWW, StroThreshNeeded=StroThreshNeeded, stroAmpThresh=stroAmpThresh,
                     stroCutoffs=stroCutoffs, rollmeanStroWW=rollmeanStroWW, writeRes=FALSE, bmpwidth=bmpwidth, bmpheight=bmpheight, ...)
  }
  if(confirmRevised) {
    dev.new(width = 30, height = 7)
    identifyStrophes(envFine, wavfile=wavfile, stropath=stropath, toFile=FALSE, 
                     wl=wl, ovlp=ovlp, padding=padding, MinStroDur=MinStroDur,
                     StroThreshExceedWW=StroThreshExceedWW, StroThreshNeeded=StroThreshNeeded, stroAmpThresh=stroAmpThresh,
                     stroCutoffs=stroCutoffs, rollmeanStroWW=rollmeanStroWW, writeRes=FALSE, ...)
    print("Check revised plot and press RETURN when done...")
    scan()
    dev.off()
  }
  return(stroCutoffs=stroCutoffs)
}
