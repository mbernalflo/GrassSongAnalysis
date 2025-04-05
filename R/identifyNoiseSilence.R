#' Identifies periods of noise and silence inside a strophe.
#' 
#' Identifies noise periods and silence periods using amplitude changes. Plots the stroph identified using \code{\link{identifyStrophes}} at the top and the difference between the fine and the coarse level at the bottom. It creates a \code{data.frame} with the starting and ending points of the noise and the silence, the duration of the noise and the silence, and the validity of the syllable.
#' 
#'
#' @param envFine A sound envelope as a vector of numbers.
#' @param wavfile Name of wavfile (required).
#' @param syllpath Path to folder in which jpg files should be plotted 
#'   (optional, working directory if missing). Directory will be created if 
#'   it does not exist.
#' @param identify Boolean. Whether or not syllable boundaries should be identified automatically.
#' @param plot Boolean, whether or not a plot should be in created.
#' @param f Sampling rate (extracted from wavfile if NULL).
#' @param toFile Boolean, whether or not the plot should be in a .jpg file 
#'   (if TRUE) or in a plotting window (if FALSE).
#' @param bmpwidth Width of the jpg image (in pixels).
#' @param bmpheight Height of the jpg image (in pixels).
#' @param rollmeanWW A number, (preferentially odd). The rolling mean window width for determining the coarse envelope from the finest resolution envelope determined by \code{wl} and \code{ovlp}. The code works more accurately for odd numbers!
#' @param rollmeanWWdiff A number (preferentially odd). The rolling mean window width for smoothing the difference between fine and coarse envelope (for syllable identification). The code works more accurate for odd numbers!
#' @param wl Window length for the (finest) sound envelope given as the 
#'   number of sample points, At f=44100: 44 eq 1 ms (recommended), 441 eq 10 
#'   ms, 4410 eq 100 ms.
#' @param ovlp overlap between two successive windows (in percent, i.e., 0).
#' @param meanReduceFrac Number (range 0-1) giving the fractional reduce of the average threshold towards the mean for syllable identification.
#' @param addThresh parameter to slightly change the threshold that defines when a Noise section starts and ends.
#' @param max_gap Maximum amount of points below the lower threshold there can be in the smoothed out vector before a cut is made.
#' @param minSyllDur Min syllable duration in seconds.
#' @param maxSyllDur Max syllable duration in seconds.
#' @param stroCutoffs A \code{data.frame} containing the strophe cutoff locations as returned by \code{identifyStrophes}.
#' @param syllCutoffs 	A \code{data.frame} containing the syllable cutoff locations (useful if they have been manually curated).
#' @param mar A vector of four numbers giving the margins of the plot.
#' @param cex Size of the syllable counter on the plot.
#' @param lwdSyllSep Line width of the syllable separator lines.
#' @param writeRes Boolean. Whether or not the strophe cutoffs should be 
#'   written to file.
#' @param overwriteRes Boolean. This is just a safty catch. If  \code{write = TRUE}
#'   and \code{overwriteRes = FALSE} the function will stop in order not to 
#'   overwrite previous manual edits.
#' @param useResFile Boolean. A check is done, if a results file already exists and
#'   the results of this file are used in \code{useResFile = TRUE}
#' 
#' @details
#' The algorithm in brief:
#' \itemize{
#'  \item Creates a coarse envelope by calculating the rolling mean of \code{envFine} for windows of \code{rollmeanWW} width (for smoothing).  
#'  \item Calculate for each point the difference between the fine and the coarse envelope and smooth that vector too using the rolling mean again, this time using the \code{rollmeanWWdiff} parameter.
#'  \item Create a threshold that depends on the mean value of this last smoothed envelope to determine whether the \code{diffSyll} amplitude is high enough to indicate that a noise period is starting.
#'  \item Use \code{max_gap} to determine the maximum amount of points belowed the threshold that would be ignored inside a sequence.
#'  \item Do some plotting and packaging of the results.
#' }
#' 
#' @return 
#' Returns a \code{data.frame}:
#' \describe{
#' \item{sysllCutoffs}{A \code{data.frame} of detected strophe cutoff points. 
#'   The elements are:
#'   \describe{
#'      \item{SyllCounter}{A counter for syllables within the strophe.}
#'      \item{noiseStartSamp}{The noise start as the sample in 
#'        \code{envFine} in the padded sequence.}
#'      \item{noiseEndSamp}{The noise end as the sample in \code{envFine} 
#'        in the padded sequence.}
#'      \item{noiseStartSec}{The noise start converted to seconds of the song 
#'        without padding.}
#'      \item{noiseEndSec}{The noise end converted to seconds of the song 
#'        without padding.}
#'      \item{noiseDurSec}{The noise duration in seconds.}
#'      \item{SyllValid}{Indicator if the strophe is valid (i.e., exceeds 
#'        \code{minStroDur}).}
#'      \item{silenceStartSamp}{The silence start as the sample in 
#'        \code{envFine} in the padded sequence.}
#'      \item{silenceEndSamp}{The silence end as the sample in \code{envFine} 
#'        in the padded sequence.}
#'      \item{silenceStartSec}{The silence start converted to seconds of the song.}
#'      \item{silenceEndSec}{The silence end converted to seconds of the song.}
#'      \item{silenceDur}{The silence duration in seconds.}
#'    }
#'   }
#' }
#' 
#' @note 
#' If the syllable identification process returns no values there willl be no plot and a message will appear on the console.
#' 
#' @author Holger Schielzeth  (holger.schielzeth@@uni-jena.de), Mar Bernal Flo (mar.bernal.flo@@uni-jena.de)
#'  
#' @seealso 
#' \code{\link{loadSongfile}}, \code{\link{curateStrophes}}, 
#' \code{\link{identifyStrophes}}, \code{\link{measureSyllables}}, 
#' \code{\link{identifyPulses}}, \code{\link{curateSyllables}}, \code{\link{mergePhrases}}, \code{\link{identifySyllables}}
#' 
#' @examples   
#' 
#' \dontrun{
#' syllpath <- "C:/mypath/"
#' wavfile <- "myfile.wav"
#' 
#' syllCutoffs <- identifyNoiseSilence(
#' envFine = envFine,
#' wavfile, 
#' plot=TRUE,
#' rollmeanWW=601,
#' rollmeanWWdiff=101,
#' max_gap=10,
#' meanReduceFrac=0.4,
#' minSyllDur=0.06,
#' maxSyllDur=1,
#' stroCutoffs=stroCutoffs
#' )
#' 
#' }
#' 
#' @export

identifyNoiseSilence <- function(
    envFine, 
    wavfile, 
    syllpath="", 
    identify=TRUE,
    plot=FALSE,
    f=44100,
    toFile=TRUE,
    bmpwidth=3200,
    bmpheight=1200,
    rollmeanWW=601,
    rollmeanWWdiff=101,
    wl=44,
    ovlp=50,
    max_gap=8,
    meanReduceFrac=0.4,
    addThresh=0,
    minSyllDur=0.06,
    maxSyllDur=1,
    stroCutoffs=NULL, 
    syllCutoffs=NULL,
    mar=NULL,
    cex=2,
    lwdSyllSep=2,
    writeRes=TRUE, 
    overwriteRes=FALSE,
    useResFile=TRUE
){
  if(any(rollmeanWW %% 2 == 0)) warning("Roll means window width should be odd numbers for higher accuracy. Found for rollmeanWW.")
  if(any(rollmeanWWdiff %% 2 == 0)) warning("Roll means window width should be odd numbers for higher accuracy. Found for rollmeanWWdiff.")
  if(syllpath!="") {
    if (!dir.exists(syllpath)) {
      dir.create(syllpath)
      print(paste0("Directory ", syllpath, " did not exist and was created."))
    }
  }
  
  
  stroCutoffs <- stroCutoffs[stroCutoffs$StroValid == 1,]
  
  if(nrow(stroCutoffs)>=1){
    syllCutoffs_list <- list()
    for(i in 1:nrow(stroCutoffs)){
      #### check if file with syllable cutoffs exists ####
      readfile <- paste0(syllpath,unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],"_Strophe", stroCutoffs$StroCounter[i], "_Syllables.txt")
      if(useResFile && file.exists(readfile)) {
        syllCutoffs <- read.table(file=readfile, sep="\t", header=TRUE)
        syllCutoffs_list[[i]] <- syllCutoffs
        identify <- FALSE
        print("Syllable identification: Existing file was read and used for cutoffs! identify was set to FALSE!")
      }
      
      if(identify || plot){
        envCoar <- zoo::rollmean(envFine, rollmeanWW, align="center")
        envCoar <- c(rep(envCoar[1], (rollmeanWW-1)/2), envCoar, rep(envCoar[length(envCoar)], (rollmeanWW-1)/2))
        truncEnvStroph <- function(x, start, end, samp=1){
          x <- x[I(start*samp):I(end*samp)]
        } 
        start <- stroCutoffs$StartSampWithPad[i]
        if(start<0) start = 0
        end <- stroCutoffs$EndSampWithPad[i]
        envFineTrunc <- truncEnvStroph(envFine, start, end)
        envCoarTrunc <- truncEnvStroph(envCoar, start, end)
        mytime <- length(envFineTrunc)/(f/(wl*(1-ovlp/100)))
        standFactor <- function(x) {max(x)/x}
        diffSyll <- envFineTrunc * standFactor(envCoarTrunc)
        
        # Syllable and pulse rollmeans
        rollmeansSyll <- zoo::rollmean(diffSyll,  rollmeanWWdiff, align="center")
        rollmeansSyll <- c(rep(rollmeansSyll[1], (rollmeanWWdiff-1)/2), rollmeansSyll, rep(rollmeansSyll[length(rollmeansSyll)], (rollmeanWWdiff-1)/2))
      
        meanReduceSyll <- (mean(rollmeansSyll)-min(rollmeansSyll))*meanReduceFrac
        #maybe add if in case the meanReduceSyll gets lower than the min(rollmeansSyll)
      
        lowerThreshold <- (mean(rollmeansSyll)-meanReduceSyll) + addThresh #threshold used for cutting syllables
        upperThreshold <- mean(rollmeansSyll)+meanReduceSyll
      
        envSyllExcThresh <- as.numeric(rollmeansSyll>lowerThreshold)  
      }
      
      if(identify){
        syllCutoffs <- which(envSyllExcThresh==1)
        
        #### select actual cutoffs #### 
        
        cuts = syllCutoffs[1]
        index = 1
        while( index != length(syllCutoffs) ){
          index = index + 1
          while( (syllCutoffs[index] - syllCutoffs[index-1]) < max_gap){
            if(index != length(syllCutoffs)){
              index = index + 1
            } else { break }
          }
          if(index != length(syllCutoffs)){
            cuts = c(cuts, syllCutoffs[c(index-1,index)])
          } else {
            cuts = c(cuts, syllCutoffs[index])
          }
        }
        
        syllCutoffs <- cuts
        
        if(length(syllCutoffs)%%2 != 0) syllCutoffs <- syllCutoffs[-length(syllCutoffs)]
        
        #### prepare dataframe #####
        if(length(syllCutoffs) == 0) {
          print(paste("No syllables found in Strophe", i))
          syllCutoffs <- NULL
        }else{
          syllCutoffs = data.frame(
            SyllCounter=1:I(length(syllCutoffs)/2),
            noiseStartSamp=syllCutoffs[seq(1,length(syllCutoffs),by=2)],
            noiseEndSamp=syllCutoffs[seq(2,length(syllCutoffs),by=2)]
          )
          syllCutoffs$noiseStartSec <- syllCutoffs$noiseStartSamp/(f/(wl*(1-ovlp/100)))
          syllCutoffs$noiseEndSec <- syllCutoffs$noiseEndSamp/(f/(wl*(1-ovlp/100)))
          syllCutoffs$noiseDurSec <- syllCutoffs$noiseEndSec - syllCutoffs$noiseStartSec
          syllCutoffs$SyllValid <- as.numeric(syllCutoffs$noiseDurSec > minSyllDur)
          syllCutoffs$silenceStartSamp <- syllCutoffs$noiseEndSamp
          syllCutoffs$silenceEndSamp <- c(syllCutoffs$noiseStartSamp[-1],(stroCutoffs$EndSampWoutPad[i] - stroCutoffs$StartSampWoutPad[i])) 
          syllCutoffs$silenceStartSec <- syllCutoffs$noiseEndSec
          syllCutoffs$silenceEndSec <- c(syllCutoffs$noiseStartSec[-1],(stroCutoffs$EndSec[i] - stroCutoffs$StartSec[i])) 
          syllCutoffs$silenceDurSec <- pmax((syllCutoffs$silenceEndSec - syllCutoffs$silenceStartSec), 0)
          syllCutoffs$maxAmpNoise <- mapply(function(noise_start, noise_end) max(envFineTrunc[noise_start:noise_end]), syllCutoffs$noiseStartSamp, syllCutoffs$noiseEndSamp)
          syllCutoffs$avgAmpSilence <- mapply(function(silence_start, silence_end) mean(envFineTrunc[silence_start:silence_end]), syllCutoffs$silenceStartSamp, syllCutoffs$silenceEndSamp)
          
        }
        syllCutoffs_list[[i]] <- syllCutoffs
      }
      #### plotting section ####
      
      if(!is.null(syllCutoffs)){
        if(plot==TRUE){
          if(toFile==TRUE){
            jpeg(filename=paste0(syllpath,unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],"_Strophe", stroCutoffs$StroCounter[i], ".jpg"), width=bmpwidth, height=bmpheight)
          }
          par(mfrow=c(2,1))
          if(!is.null(mar)) {par(mar=mar)}
          xlimFullPlot <- c(0,length(envFineTrunc))
          xlimSyllPlot <- c(0,length(envFineTrunc))
          offsetRollMeanPlotSyll = (max(envFineTrunc)/2)
          offsetRollMeanPlotPuls = 100
          
          # (1) Raw plot
          plot(envFineTrunc, type="l", xaxt="n", xlab="Time (sec)", col="red", xlim=xlimFullPlot)
          points(envCoarTrunc, type="l", col="black")
          abline(h=stroAmpThresh)
          axis(1, at=seq(0, mytime, by=0.5)*(f/(wl*(1-ovlp/100))), labels=seq(0, mytime, by=0.5))
          if(!is.null(syllCutoffs)) {
            abline(v=syllCutoffs$noiseStartSamp, col="blue", lty=-1*syllCutoffs$SyllValid+2, lwd=lwdSyllSep)
            abline(v=syllCutoffs$noiseEndSamp, col="blue", lty=-1*syllCutoffs$SyllValid+2, lwd=lwdSyllSep)
          }
          # (2) SyllDiff plot 
          plot (diffSyll, type="l", xaxt="n", col="blue", xlim=xlimSyllPlot)
          axis(1, at=seq(0, mytime, by=0.5)*(f/(wl*(1-ovlp/100))), labels=seq(0, mytime, by=0.5))
          text(syllCutoffs$noiseStartSamp, max(diffSyll)*0.90, labels=syllCutoffs$SyllCounter, pos=4, cex=cex, col=c("grey90", "red")[syllCutoffs$SyllValid+1])
          points(rollmeansSyll+offsetRollMeanPlotSyll, type="b", col="lightblue")
          abline(h=(lowerThreshold + c(0,offsetRollMeanPlotSyll)), lty=2, col=c("blue", "lightblue"))
          abline(h=(upperThreshold + c(0,offsetRollMeanPlotSyll)), lty=2, col=c("blue", "lightblue"))
          if(!is.null(syllCutoffs)) {
            abline(v=syllCutoffs$noiseStartSamp, col="blue", lty=-1*syllCutoffs$SyllValid+2, lwd=lwdSyllSep)
            abline(v=syllCutoffs$noiseEndSamp, col="blue", lty=-1*syllCutoffs$SyllValid+2, lwd=lwdSyllSep)
          }
          par(mfrow=c(1,1))
          if(toFile==TRUE) {dev.off()}
        }
      } else {print("No image produced because syllCutoffs==0")}
      
      if(writeRes){
        writefile = paste0(syllpath,unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],"_Strophe", stroCutoffs$StroCounter[i], "_Syllables.txt")
        if(file.exists(writefile) & overwriteRes==FALSE){
          print("Syllable identification: File not written, because it already exists! Set overwriteRes = TRUE if you are sure you want to replace the existing file!")
        }else{
          write.table(syllCutoffs, paste0(syllpath,unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],"_Strophe", stroCutoffs$StroCounter[i], "_Syllables.txt"), sep="\t", quote=FALSE, row.names=FALSE)
        }
        
      }
    }
    return(syllCutoffs_list)
  }

}
