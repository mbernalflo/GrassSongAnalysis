#' Plot grasshopper song spectra,
#' 
#' Plots a song spectrum and oscilogram from \code{\link{Wave}} objects.
#'
#' @param song An Wave object to be analysed.
#' @param wavfile Name of wavfile (required).
#' @param specpath Path to folder in wich bmp files should be plotted 
#'   (optional, working directory if missing).
#' @param f Sampling rate (extracted from wavfile if NULL).
#' @param flim Modifications of the frequency Y-axis limits (in kHz, integers 
#'   only) for display.
#' @param toFile Boolean, whether or not the plot should be in a .bmp file (if 
#'   TRUE) or in a plotting window (if FALSE).
#' @param stroCutoffs Optional data.frame of strophe cutoffs (as returned from 
#'   \code{\link{identifyStrophes}}).
#' @param wlSpec Window width for the spectrogram (number of sample points, at 
#'   f=44100: 4410 eq 100 ms, 441 eq 10 ms, 44.1 eq 1 ms).
#' @param ovlpSpec Overlap between two successive windows for spectrogram  (in 
#'   percent, i.e. 0-99).
#' @param wlFreq Window width for the analysis of dominant frequency (number of 
#'   sample points, at f=44100: 4410 eq 100 ms, 441 eq 10. ms, 44.1 eq 1 ms).
#' @param ovlpFreq Overlap between two successive windows for the analysis of 
#'   dominant frquency  (in percent, i.e. 0-99).
#' @param minAmpFreq Minimum amplitude required for analysing the dominant 
#'   frequncy (in proportion of maximum, i.e. 0-1).
#' @param bmpwidth Width of the bmp image (in pixels).
#' @param bmpheight Height of the bmp image (in pixels).
#' @param specNorm Boolean, whether or not the spectrum should be normalized.
#' @param specScale Boolean, whether or not the scale bar should be plotted 
#'   with the spectrogram.
#' @param listen Boolean, whether or not the song should be played after 
#'   plotting.
#' 
#' @return 
#' Returns a list.
#' 
#' @references 
#' My references
#' 
#' @author Holger Schielzeth (holger.schielzeth@@uni-bielefeld.de).
#' 
#' @seealso 
#' \code{\link{loadSongfile}}, \code{\link{curateStrophes}}, 
#' \code{\link{identifyStrophes}}, \code{\link{identifySyllables}}, 
#' \code{\link{measureSyllables}}, \code{\link{curateSyllables}}.
#' 
#' @examples   
#' 
#' \dontrun{
#' 
#' mysound = filePreparation("myfile.wav", "C://mypath//", highpassfilter=1001, select=c(0.5,2))
#' myspec = plotSpectro(mysound, "myfile.wav", "C://mypath//")       
#' }
#' 
#' @export

plotSpectro <- function(song, wavfile, specpath="", f=NULL, flim=22, toFile=TRUE, stroCutoffs=NULL,
                        wlSpec=441, ovlpSpec=50, wlFreq=441, ovlpFreq=75, minAmpFreq = 0.25,
                        bmpwidth=3200, bmpheight=600, specNorm=TRUE, specScale=FALSE, listen=FALSE) {
  # Initial checks
	if(class(song) != "Wave") stop(paste0(wavfile, "has to be a Wave object."))
	if(is.null(f)) f=song@samp.rate #/2
	# Setup of bmp file if necessary
  if(toFile) {
	  bmp(filename=paste0(specpath,unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],".bmp"), width=bmpwidth, height=bmpheight)
	  on.exit(dev.off())
	}
	
	# Plotting of spectrum and sound envelopes
	myspec = seewave::spectro(song, f=f, norm=specNorm, scale=specScale, wl=wlSpec, ovlp=ovlpSpec, flim=c(0,flim), plot=TRUE,
                            osc=TRUE, grid=FALSE, main=wavfile, listen=listen)
  if(!is.null(stroCutoffs)) {
    abline(v=stroCutoffs$StartSec, lty=1, col="black")
    abline(v=stroCutoffs$EndSec,   lty=2, col="black")    
  }
	par(new=TRUE)	
	domfreq = seewave::dfreq(song, f=f, wl=wlFreq, ovlp=ovlpFreq, clip=minAmpFreq, plot=TRUE, col="red", pch=19, cex=1.5)	
	return(list(myspec=myspec, domfreq=domfreq))	
}