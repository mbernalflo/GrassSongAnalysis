#' Plot strophes and identify syllables and pulses
#' 
#' Plot pre-identified strophes  at the three levels in one plot, the 
#' difference between the coarsest and intermediate level in a second plot (for 
#' syllable identification) and the differeence between finest and intermidiate 
#' level in a third plot (in a third plot). Also applies automated syllable and 
#' strophe identification.
#'
#' @param envFine A sound envelope as a vector of numbers.
#' @param wavfile Name of wavfile (necessary).
#' @param syllpath Path to folder in wich jpg files should be plotted (optional, 
#'   working directory if missing).
#' @param identify Boolean. Whether or not syllable and puls boundaries should 
#'   be identified automatically.
#' @param plot Boolean. Whether or not the data should be plotted.
#' @param f Sampling rate (extracted from wavfile if NULL).
#' @param toFile Boolean, whether or not the plot should be in a .jpg file (if 
#'   TRUE) or in a plotting window (if FALSE).
#' @param bmpwidth Width of the jpg image (in pixels).
#' @param bmpheight Height of the jpg image (in pixels).
#' @param rollmeanWW Vector of 2 (preferentially odd) numbers. The rolling mean 
#'   window width for determining the coarse and intermediate envolopes from 
#'   the finest resultion envolope determined by \code{wl} and \code{ovlp}. The 
#'   code works more accurate for odd numbers!
#' @param rollmeanWWdiff Vector of 2 (preferentially odd) numbers. The rolling 
#'   mean window width for smoothing the difference between intermediate and 
#'   coarse envolope (first value, for syllable identification) and fine and 
#'   intermediate level (second value, for pulse indentification). The code 
#'   works more accurate for odd numbers!
#' @param meanReduceFrac Vector of 2 numbers (range 0-1) giving the fractional 
#'   reduce of the average threshold towards the mean for syllable (first 
#'   number) and puls (second number) identification.
#' @param tol Tolerance for the detection of minima (handed over to 
#'   \code{\link{peakIdent}}). Should be some low value or 0.
#' @param wl Window length used when determining the sound envelope (used here 
#'   only for back-conversion to time in seconds).
#' @param ovlp Overlap used when determining the sound envelope (used here only
#'   for back-conversion to time in seconds).
#' @param stroAmpThresh Envelope threshold for identification of silence/sound 
#'   (used here only for plotting).
#' @param stroCutoffs A \code{data.frame} containing the strophe cutoff 
#'   locations as returned by \code{\link{identifyStrophes}}.
#' @param minSyllDur Min syllable duration in seconds.
#' @param maxSyllDur Max syllable duration in seconds.
#' @param syllCutoffs A \code{data.frame} containing the syllable cutoff 
#'   locations as returned by \code{\link{identifyStrophes}} (useful if they 
#'   have been manually curated).
#' @param xlimFullPlot A vector of two numbers that determine the x limits 
#'   (given in units of samples) to the full plot (for zooming in). If NULL the
#'   full range is plotted.
#' @param xlimSyllPlot A vector of two numbers that determine the x limits 
#'   (given in units of samples) to the syllable plot (for zooming in). If NULL 
#'   the full range is plotted.
#' @param xlimSumPlot A vector of two numbers that determine the x limits 
#'   (given in units of samples) to the summary plot (for zooming in). If NULL 
#'   the full range is plotted.
#' @param mCleanIt Number of interations for the second cleaning filter 
#'   (removing minima, where the rolling mean has not exceeded the average 
#'   amplituted difference).
#' @param mar A vector of four numbers giving the margins of the plot.
#' @param cex Size of the syllable counter on the plot.
#' @param lwdSyllSep Line width of the syllable seperator lines.
#' @param writeRes Boolean. Whether or not the strophe cutoffs should be 
#'   written to file.
#' @param overwriteRes Boolean. This is just a safty catch. If  \code{write = TRUE}
#'   and \code{overwriteRes = FALSE} the function will stop in order not to 
#'   overwrite previous manual edits.
#' @param useResFile Boolean. A check is done, if a results file already exists and
#'   the results of this file are used in \code{useResFile = TRUE}
#' 
#' @return 
#' Returns the a list.
#' \item{syllCutoffs}{A \code{data.frame} of detected syllable cutoff points. The elements are:
#'   \describe{
#'      \item{StartSampWithPad}{The strophe start as the sample in \code{envFine} in the padded sequence.}
#'      \item{EndSampWithPad}{The strophe end as the sample in \code{envFine} in the padded sequence.}
#'      \item{StartSampWoutPad}{The strophe start as the sample in \code{envFine} in sequence without padding.}
#'      \item{EndSampWoutPad}{The strophe end as the sample in \code{envFine} in sequence without padding.}
#'      \item{StartSec}{The strophe start converted to seconds of the song without padding.}
#'      \item{EndSec}{The strophe end converted to seconds of the song without padding.}
#'      \item{DurSec}{The strophe duration in seconds.}
#'      \item{StroCounter}{A counter for strophes within the \code{song}.}
#'    }
#'  }
#' \item{pulsSummary}{A \code{data.frame} of detected puls cutoff points. The elements are:
#'   \describe{
#'      \item{StartSampWithPad}{The strophe start as the sample in \code{envFine} in the padded sequence.}
#'      \item{EndSampWithPad}{The strophe end as the sample in \code{envFine} in the padded sequence.}
#'      \item{StartSampWoutPad}{The strophe start as the sample in \code{envFine} in sequence without padding.}
#'      \item{EndSampWoutPad}{The strophe end as the sample in \code{envFine} in sequence without padding.}
#'      \item{StartSec}{The strophe start converted to seconds of the song without padding.}
#'      \item{EndSec}{The strophe end converted to seconds of the song without padding.}
#'      \item{DurSec}{The strophe duration in seconds.}
#'      \item{StroCounter}{A counter for strophes within the \code{song}.}
#'    }
#'  }
#'  
#' @references 
#' My references
#' 
#' @author Holger Schielzeth  (holger.schielzeth@@uni-bielefeld.de)
#' 
#' @seealso 
#' \code{\link{loadSongfile}}, \code{\link{curateStrophes}}, 
#' \code{\link{identifyStrophes}}, \code{\link{measureSyllables}}, 
#' \code{\link{identifyPulses}}, \code{\link{curateSyllables}}.
#' 
#' @importFrom pracma findpeaks
#' @examples   
#' 
#' \dontrun{
#' 
#' mysound = filePreparation("myfile.wav", "C://mypath//", highpassfilter=1001, select=c(0.5,2))
#' myenv = plotEnvolopes(mysound, "myfile.wav", "c://mystropath//", padding = 300)
#' mystro = plotStrophes(myenv$envFine, "myfile.wav", "C://mypath//")
#' }
#' 
#' 
#' @export

identifySyllables <- function(envFine, wavfile, syllpath="", identify=TRUE, plot=TRUE, f=44100, 
                              toFile=TRUE, bmpwidth=3200, bmpheight=1200,
                              rollmeanWW=c(11, 101), rollmeanWWdiff=c(51, 1), 
                              tol=1e-7, wl=44, ovlp=50, meanReduceFrac=c(0.4,0.1), mCleanIt=3,
                              minSyllDur=0, maxSyllDur=1,
                              stroAmpThresh=15, stroCutoffs=NULL, syllCutoffs=NULL, 
                              xlimFullPlot=NULL, xlimSyllPlot=NULL, xlimSumPlot=NULL, mar=NULL, cex=2, lwdSyllSep=2,
                              writeRes=TRUE, overwriteRes=FALSE, useResFile=TRUE) {
  
  if(any(rollmeanWW %% 2 == 0)) warning("Roll means window width should be odd numbers for higher accuracy. Found for rollmeanWW.")
  if(any(rollmeanWWdiff %% 2 == 0)) warning("Roll means window width should be odd numbers for higher accuracy. Found for rollmeanWWdiff.")
  if(syllpath!="") {
    if (!dir.exists(syllpath)) {
      dir.create(syllpath)
      print(paste0("Directory ", syllpath, " did not exist and was created."))
    }
  }
  
  stroCutoffs = stroCutoffs[stroCutoffs$StroValid == 1,]
  if(nrow(stroCutoffs)>=1) {
    for(i in 1:nrow(stroCutoffs)) {
      readfile = paste0(syllpath,unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],"_Strophe", stroCutoffs$StroCounter[i], "_Syllables.txt")
      if(useResFile && file.exists(readfile)) {
        syllCutoffs = read.table(file=readfile, sep="\t", header=TRUE)
        identify = FALSE
        print("Syllable identification: Existing file was read and used for cutoffs! identify was set to FALSE!")
      }
      
      if(identify==FALSE) meanReduceSyll = 0
      
      if(identify || plot) {
        # calculate rollmeans envMedi and envCoar
        envCoar = zoo::rollmean(envFine, rollmeanWW[1], align="center")
        envCoar = c(rep(envCoar[1], (rollmeanWW[1]-1)/2), envCoar, rep(envCoar[length(envCoar)], (rollmeanWW[1]-1)/2))
        #$    envMedi = zoo::rollmean(envFine, rollmeanWW[2], align="center")
        #$    envMedi = c(rep(envMedi[1], (rollmeanWW[2]-1)/2), envMedi, rep(envMedi[length(envMedi)], (rollmeanWW[2]-1)/2))
  
        # truncation of envelopes to strophe window
        truncEnvStroph = function(x, start, end, samp=1){x = x[I(start*samp):I(end*samp)]}
        start   = stroCutoffs$StartSampWithPad[i]
          if(start<0) start = 0
        end     = stroCutoffs$EndSampWithPad[i]
        #samp    = f / (wl[1]*(1-ovlp[1]/100))
        envFineTrunc = truncEnvStroph(envFine, start, end)
        #$    envMediTrunc = truncEnvStroph(envMedi, start, end)
        envCoarTrunc = truncEnvStroph(envCoar, start, end)
        mytime = length(envFineTrunc)/(f/(wl*(1-ovlp/100))) #$

        # Calculation of standardized differences
        standFactor = function(x) {max(x)/x}
        diffSyll = envFineTrunc * standFactor(envCoarTrunc)  # IMPORTANT CHANGE HERE! envMediTrunc to envFineTrunc
    
        # Syllable and pulse rollmeans
        rollmeansSyll <- zoo::rollmean(diffSyll,  rollmeanWWdiff[1], align="center")
        rollmeansSyll <- c(rep(rollmeansSyll[1], (rollmeanWWdiff[1]-1)/2), rollmeansSyll, rep(rollmeansSyll[length(rollmeansSyll)], (rollmeanWWdiff[1]-1)/2))
      }
      
      #### Syllable identification #####
      if(identify==TRUE) {
        meanReduceSyll <- (mean(rollmeansSyll)-min(rollmeansSyll))*meanReduceFrac[1]
        rollmeanSignSyll <- as.numeric(rollmeansSyll<=mean(rollmeansSyll)-meanReduceSyll)
        
        #### try ####
        valleys <- pracma::findpeaks(-rollmeansSyll, threshold=0.5)
        print(valleys)
        #############
        
        syllCutoffs <- peakIdent(rollmeansSyll, tol=tol, decreasing=TRUE)
        # Cleaning Filter 1
        syllCutoffsValid = as.numeric(rollmeanSignSyll[syllCutoffs]==1)
        syllCutoffs      = syllCutoffs[syllCutoffsValid==1]
        # Cleaning Filter 2
        for(k in 1:mCleanIt) { # Hidden switch
          syllCutoffsValid = rep(1,length(syllCutoffs))
          rollmeanSignSyllAvg = as.numeric(rollmeansSyll>mean(rollmeansSyll))
          for(j in 2:length(syllCutoffs)) {
            if(sum(rollmeanSignSyllAvg[syllCutoffs[j-1]:syllCutoffs[j]]) == 0)             {
             if(envFine[syllCutoffs[j-1]] < envFine[syllCutoffs[j]]) syllCutoffsValid[j]   = 0
             if(envFine[syllCutoffs[j-1]] > envFine[syllCutoffs[j]]) syllCutoffsValid[j-1] = 0
            }
          }  
          syllCutoffs <- syllCutoffs[syllCutoffsValid==1]
        }    
        if(rollmeanWWdiff[1]>1) {
          for(j in 1:length(syllCutoffs)) {
            rollmeanWind <- syllCutoffs[j] + c(-1,1) * (rollmeanWWdiff[1]-1)/2
            diffSyllwindow <- envFine[rollmeanWind[1]:rollmeanWind[2]]  # IMPORTANT CHANGE HERE diffSyll to envFine
            shift <- which(diffSyllwindow == min(diffSyllwindow, na.rm=TRUE)) - 1 - (rollmeanWWdiff[1]-1)/2
            if(length(shift)>1){ 
              shift <- shift[sort(abs(shift), index.return=TRUE)$ix][1]
            }
            syllCutoffs[j] = syllCutoffs[j]+shift
          }
        }    
        syllCutoffs      = data.frame(SongFile=wavfile, StroCounter = stroCutoffs$StroCounter[i], StroWindOffSamp=stroCutoffs$StartSampWoutPad[i], SyllCounter=1:I(length(syllCutoffs)-1),  SyllCounterRev=rev(1:I(length(syllCutoffs)-1)), SyllPosProp=1:I(length(syllCutoffs)-1)/length(syllCutoffs), SyllBegSamp=syllCutoffs[-length(syllCutoffs)], SyllEndSamp=syllCutoffs[-1])
        syllCutoffs$SyllDurationSamp = syllCutoffs$SyllEndSamp-syllCutoffs$SyllBegSamp
        syllCutoffs$SyllDurationSec  = syllCutoffs$SyllDurationSamp / ( f / (wl*(1-ovlp/100)))    
        syllCutoffs$SyllValid = as.numeric(syllCutoffs$SyllDurationSec >= minSyllDur & syllCutoffs$SyllDurationSec <= maxSyllDur)
        print(as.numeric(syllCutoffs$SyllDurationSec >= minSyllDur & syllCutoffs$SyllDurationSec <= maxSyllDur))
      }
              
      #### Plotting section ####
      if(plot==TRUE) {
        if(toFile==TRUE) jpeg(filename=paste0(syllpath,unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],"_Strophe", stroCutoffs$StroCounter[i], ".jpg"), width=bmpwidth, height=bmpheight)
        par(mfrow=c(2,1))
        if(!is.null(mar)) par(mar=mar)
        if(is.null(xlimFullPlot)) xlimFullPlot = c(0,length(envFineTrunc))
        if(is.null(xlimSyllPlot)) xlimSyllPlot = c(0,length(envFineTrunc))
        #if(is.null(xlimPulsPlot)) xlimPulsPlot = c(0,length(envFineTrunc))
        if(is.null(xlimSumPlot))  xlimSumPlot = c(0,length(envFineTrunc))
        offsetRollMeanPlotSyll = 80
        offsetRollMeanPlotPuls = 100
      
        # (1) Raw plot
        plot  (envFineTrunc, type="l", xaxt="n", xlab="Time (sec)", col="red", xlim=xlimFullPlot)
  #$      points(envMediTrunc, type="l", col="blue")
        points(envCoarTrunc, type="l", col="black")
        abline(h=stroAmpThresh)
        axis(1, at=seq(0, mytime, by=0.5)*(f/(wl*(1-ovlp/100))), labels=seq(0, mytime, by=0.5))
        if(!is.null(syllCutoffs)) abline(v=syllCutoffs$SyllBegSamp, col="blue", lty=-1*syllCutoffs$SyllValid+2, lwd=lwdSyllSep)
        if(!is.null(syllCutoffs)) abline(v=syllCutoffs$SyllEndSamp, col="blue", lty=-1*syllCutoffs$SyllValid+2, lwd=lwdSyllSep)
        
        # (2) SyllDiff plot 
        plot (diffSyll, type="l", xaxt="n", col="blue", xlim=xlimSyllPlot)
        axis(1, at=seq(0, mytime, by=0.5)*(f/(wl*(1-ovlp/100))), labels=seq(0, mytime, by=0.5))
        text(syllCutoffs$SyllBegSamp, max(diffSyll)*0.90, labels=syllCutoffs$SyllCounter, pos=4, cex=cex, col=c("grey90", "red")[syllCutoffs$SyllValid+1])
        points(rollmeansSyll+offsetRollMeanPlotSyll, type="b", col="lightblue")
        #### try ####
        points(x=valleys[,2], y=(-valleys[,1]+offsetRollMeanPlotSyll), type="p", col="black")
        ############
        abline(h=mean(rollmeansSyll)-meanReduceSyll+c(0,offsetRollMeanPlotSyll), lty=2, col=c("blue", "lightblue"))
        abline(h=mean(rollmeansSyll)+meanReduceSyll+c(0,offsetRollMeanPlotSyll), lty=2, col=c("blue", "lightblue"))
        if(!is.null(syllCutoffs)) abline(v=syllCutoffs$SyllBegSamp, col="blue", lty=-1*syllCutoffs$SyllValid+2, lwd=lwdSyllSep)
        if(!is.null(syllCutoffs)) abline(v=syllCutoffs$SyllEndSamp, col="blue", lty=-1*syllCutoffs$SyllValid+2, lwd=lwdSyllSep)
        par(mfrow=c(1,1))
        if(toFile==TRUE) dev.off()
      } 
      #### Writing of results ####
      if(writeRes) {
        writefile = paste0(syllpath,unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],"_Strophe", stroCutoffs$StroCounter[i], "_Syllables.txt")
        if(file.exists(writefile) & overwriteRes==FALSE) print("Syllable identification: File not written, because it already exists! Set overwriteRes = TRUE if you are sure you want to replace the existing file!")
        else write.table(syllCutoffs, paste0(syllpath,unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],"_Strophe", stroCutoffs$StroCounter[i], "_Syllables.txt"), sep="\t", quote=FALSE, row.names=FALSE)
      }
    }
  }

  # Return values
	return(syllCutoffs = syllCutoffs)	
}