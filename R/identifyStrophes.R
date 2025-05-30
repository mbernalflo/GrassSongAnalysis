#' Identify and plot sound envelopes and identify strophe cutoffs.
#' 
#' Idendifies sound envelopes at two levels (on derived from the 
#' \code{\link{Wave}} object and two determined by smoothing. Plots the sound 
#' envelopes at the three levels in one plot, the difference between the 
#' coarsest and intermediate level in a second plot (for syllable 
#' identification) and the differeence between finest and intermidiate level 
#' in a third plot (in a third plot). Also applies automated strophe 
#' identification.
#'
#' @param envFine A sound envelope as a vector of numbers.
#' @param wavfile Name of wavfile (required).
#' @param stropath Path to folder in which jpg files should be plotted 
#'   (optional, working directory if missing). Directory will be created if 
#'   it does not exist.
#' @param f Sampling rate (extracted from wavfile if NULL).
#' @param plot Boolean, whether or not a plot should be in created.
#' @param toFile Boolean, whether or not the plot should be in a .jpg file 
#'   (if TRUE) or in a plotting window (if FALSE).
#' @param bmpwidth Width of the jpg image (in pixels).
#' @param bmpheight Height of the jpg image (in pixels).
#' @param rollmeanStroWW Vector of 2 (preferentially odd) numbers. The 
#'   rolling mean window width for determining the intermediate an coarse 
#'   envolopes from the finest resultion envolope determined by \code{wl} and 
#'   \code{ovlp}. The code works more accurate for odd numbers!
#' @param wl Window length for the (finest) sound envelope given as the 
#'   number of sample points, At f=44100: 44 eq 1 ms (recommended), 441 eq 10 
#'   ms, 4410 eq 100 ms.
#' @param ovlp overlap between two successive windows (in percent, i.e., 0.
#' @param stroAmpThresh Envelope threshold for identification of silence/sound 
#'   (i.e., Strophes). Recommended value depends on background noise level.
#' @param StroThreshExceedWW The number of samples that is inspected for the 
#'   identification of strophe beginning and ends.
#' @param StroThreshNeeded The number of samples within the window determined 
#'   by \code{StroThreshExceedWW} that have to exceed the threshold determined 
#'   by \code{stroAmpThresh} in order to qualify as a sound.
#' @param MinStroDur A minimum duration for potential strophes to be analyzed 
#'   (in seconds).
#' @param padding Boolean or number. If FALSE or 0, no padding will be applied. 
#'   If a number >0, padding at the beginning and end will be applied with 
#'   padding interpreted as the number of samples. If TRUE, padding at the 
#'   beginning and end will be applied with the number of samples determined 
#'   by the \code{StroThreshExceedWW - StroThreshNeeded + 1} (recommended).
#' @param stroCutoffs Optional \code{data.frame} containing strophe 
#'   cutoffpoints (as returned by \code{stroIdent}). If provided, strophe 
#'   cutoff identification will be skipped and strophe cutoffs will be plotted 
#'   as provided. Note that the padding has to be the same in the 
#'   \code{stroCutoffs} as in \code{padding}, since cutoffs will be plotted 
#'   with padding!).
#' @param writeRes Boolean. Whether or not the strophe cutoffs should be 
#'   written to file.
#' @param overwriteRes Boolean. This is just a safty catch. If  \code{write = TRUE}
#'   and \code{overwriteRes = FALSE} the function will stop in order not to 
#'   overwrite previous manual edits.
#' @param useResFile Boolean. If \code{useResFile = TRUE} a check is done to see if there are previous files that can be used as results. If there are previous files but are not to be used, set \code{useResFile = FALSE} and consider also setting \code{overwriteRes = TRUE}.
#' @param xlim Vector of two numbers used to set x-limits in plot. Useful only to 
#'   zoom in when cutoffs are know.
#' 
#' @details
#' The algorithm in brief:
#' \itemize{
#'  \item Determine sound envelope with parameters \code{wl} and \code{ovlp} 
#'    using the function \code{\link{env}} from the \code{link{seewave}} 
#'    package (using the default \code{envt = "hil"}.
#'  \item Use a sliding window to determine the smoothed envelopes.
#'  \item Extend the sliding window sequences at the ends (by simply 
#'    replicating the first and last elements) so that the smoothed envelopes 
#'    have the same length as the original.
#'  \item Search for windows of size \code{StroThreshExceedWW} in the 
#'    coaresets resolution that do/do no exceed the amplitude threshold 
#'    determined by \code{stroAmpThresh}.
#'  \item Do some plotting and packaging of the results.
#' }
#' 
#' @return 
#' Returns the a list of 2 Elements:
#' \describe{
#' \item{stroCutoffs}{A \code{data.frame} of detected strophe cutoff points. 
#'   The elements are:
#'   \describe{
#'      \item{StartSampWithPad}{The strophe start as the sample in 
#'        \code{envFine} in the padded sequence.}
#'      \item{EndSampWithPad}{The strophe end as the sample in \code{envFine} 
#'        in the padded sequence.}
#'      \item{StartSampWoutPad}{The strophe start as the sample in 
#'        \code{envFine} in sequence without padding.}
#'      \item{EndSampWoutPad}{The strophe end as the sample in \code{envFine} 
#'        in sequence without padding.}
#'      \item{StartSec}{The strophe start converted to seconds of the song 
#'        without padding.}
#'      \item{EndSec}{The strophe end converted to seconds of the song 
#'        without padding.}
#'      \item{DurSec}{The strophe duration in seconds.}
#'      \item{StroCounter}{A counter for strophes within the \code{song}.}
#'      \item{StroValid}{Idicator if the strophe is valid (i.e., exceeds 
#'        \code{MinStroDur}).}
#'    }
#'   }
#' }
#' 
#' @note 
#' Contains two work-arounds to avoid cutoffs that run in a sequence and uneven 
#' numbers of cutoffs (since usually start and end cutoffs are expected to occur 
#' in pairs).
#' 
#' @author Holger Schielzeth  (holger.schielzeth@@uni-jena.de)
#'  
#' @seealso 
#' \code{\link{loadSongfile}}, \code{\link{curateStrophes}}, 
#' \code{\link{identifySyllables}}, \code{\link{measureSyllables}}, 
#' \code{\link{identifyPulses}}, \code{\link{curateSyllables}}
#' 
#' @examples   
#' 
#' \dontrun{
#' 
#' mysound = filePreparation("myfile.wav", "C://mypath//", highpassfilter=1001, select=c(0.5,2))
#' plotEnvolopes(mysound, "myfile.wav", "c://mystropath//", padding = 300)
#' }
#' 
#' @export

identifyStrophes <- function(
    envFine,
    wavfile,
    stropath="",
    f=44100,
    plot=TRUE,
    toFile=TRUE,
    bmpwidth=3200,
    bmpheight=600,
    rollmeanStroWW=c(11, 101),
    wl=44,
    ovlp=50,
    padding=TRUE,
    stroAmpThresh=15,
    StroThreshExceedWW=301,
    StroThreshNeeded=10,
    MinStroDur=0.5,
    stroCutoffs=NULL,
    writeRes=TRUE,
    overwriteRes=FALSE,
    useResFile=TRUE,
    xlim=NA
){
  # Initial checks
  if(any(rollmeanStroWW %% 2 == 0)) warning("Roll means window width should be odd numbers for higher accuracy. Found for rollmeanStroWW.")
  if(any(StroThreshExceedWW %% 2 == 0)) warning("Roll means window width should be odd numbers for higher accuracy. Found for StroThreshExceedWW.")
  if(is.logical(padding) && padding) padding = StroThreshExceedWW - StroThreshNeeded + 1
  if(is.logical(padding) && padding ==FALSE || is.null(padding)) padding = 0
  if(stropath!="") {
    if (!dir.exists(stropath)) {
      dir.create(stropath)
      print(paste0("Directory ", stropath, " did not exist and was created."))
    }
  }
    
  readfile = paste0(stropath, unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],".txt")
  if(is.null(stroCutoffs) && useResFile && file.exists(readfile)) {
    stroCutoffs = read.table(file=readfile, sep="\t", header=TRUE)
    print("Existing file was read and used for cutoffs!")
    if(any(is.na(stroCutoffs))) {
      stroCutoffs = recalcCutoffNAs(stroCutoffs, f=f, wl=wl, ovlp=ovlp, padding=padding, file=readfile)
      print("Existing cutoffs file contained NAs that were recalcuated!")
    }
  }
  
  if(is.null(stroCutoffs) || plot==TRUE) {
    # Envelope identification
    envStro = zoo::rollmean(envFine, rollmeanStroWW, align="center")
    envStro = c(rep(envStro[1], (rollmeanStroWW-1)/2), envStro, rep(envStro[length(envStro)], (rollmeanStroWW-1)/2))
    mytime = length(envFine)/(f/(wl*(1-ovlp/100)))
  
    # Padding if requested
    if(padding > 0) {
      pad = rep(0,padding)
      envFine = c(pad, envFine, pad)
      envStro = c(pad, envStro, pad)    
    }
  }

  if(is.null(stroCutoffs)) {
    # Cutoff detection
    envStroExcThresh = as.numeric(envStro>stroAmpThresh)
    rollthreshExceed = zoo::rollmean(envStroExcThresh, StroThreshExceedWW, align="center")
    trend = rollthreshExceed[-1] - rollthreshExceed[-length(rollthreshExceed)]
    trend = c(trend[1], trend)
    stroCutoffs = as.numeric(rollthreshExceed > I(StroThreshNeeded-1)/StroThreshExceedWW & rollthreshExceed < I(StroThreshNeeded+1)/StroThreshExceedWW)
    stroCutoffs = which(stroCutoffs==1)
    stroCutoffs[stroCutoffs<padding] = padding
    stroCutoffs[stroCutoffs>=length(envStroExcThresh)-padding] = length(stroCutoffs)-padding  
    # A bit of a hack !!!!!
    if(any(diff(stroCutoffs)==1)) {
      repeat{
        stroCutoffs = stroCutoffs[which(diff(stroCutoffs)==1)[1]*-1]
        if(all(diff(stroCutoffs)>1)) break
      }
    }
    ## Even more of a hack!!!!!
    if(length(stroCutoffs)%%2 != 0) stroCutoffs = stroCutoffs[-length(stroCutoffs)]
    
    # Packaging of return object of cutoffs
    stroCutoffs = data.frame(StroCounter=1:I(length(stroCutoffs)/2), StartSampWithPad=stroCutoffs[seq(1,length(stroCutoffs),by=2)], EndSampWithPad=stroCutoffs[seq(2,length(stroCutoffs),by=2)] ) 
    stroCutoffs$StartSampWoutPad = stroCutoffs$StartSampWithPad - padding
    stroCutoffs$EndSampWoutPad   = stroCutoffs$EndSampWithPad - padding
    stroCutoffs$StartSec         = stroCutoffs$StartSampWoutPad/(f/(wl*(1-ovlp/100)))
    stroCutoffs$EndSec           = stroCutoffs$EndSampWoutPad/(f/(wl*(1-ovlp/100)))
    stroCutoffs$DurSec           = stroCutoffs$EndSec - stroCutoffs$StartSec
    stroCutoffs$StroValid        = as.numeric(stroCutoffs$DurSec > MinStroDur)
  }
  
  
  #### plotting ####
  # Setup of jpg file if necessary
  if(plot) {
    if(toFile) {
      jpeg(filename=paste0(stropath,unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],".jpg"), width=bmpwidth, height=bmpheight)
      on.exit(dev.off())
    }

    # Plotting section
    # Raw plot
    if(any(is.na(xlim))) plot(envFine, type="n", xaxt="n", xlab="Time (sec)", col="red")
   if(any(!is.na(xlim))) plot(envFine, type="n", xaxt="n", xlab="Time (sec)", col="red", xlim=xlim)
    text(stroCutoffs$StartSampWithPad, max(envFine)*0.90, labels=stroCutoffs$StroCounter, pos=4, cex=8, col=c("grey90", "red")[stroCutoffs$StroValid+1])
    points(envFine, type="l", col="red")
    points(envStro, type="l", col="black")
    abline(h=stroAmpThresh)
    axis(1, at=seq(0, mytime, by=0.5)*(f/(wl*(1-ovlp/100))), labels=seq(0, mytime, by=0.5))
    abline(v=stroCutoffs$StartSampWithPad, col="black", lwd=c(1,2)[stroCutoffs$StroValid+1], lty=1) 
    abline(v=stroCutoffs$EndSampWithPad,   col="black", lwd=c(1,2)[stroCutoffs$StroValid+1], lty=1) 
  }
  
  if(writeRes) {
    writefile = paste0(stropath, unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],".txt")
    if(file.exists(writefile) & overwriteRes==FALSE) print("File not written, because it already exists! Set overwriteRes = TRUE if you are sure you want to replace the existing file!")
    else write.table(stroCutoffs, file=writefile, sep="\t", quote=FALSE, row.names=FALSE)
  }
  # Return values
	return(stroCutoffs=stroCutoffs)	
}