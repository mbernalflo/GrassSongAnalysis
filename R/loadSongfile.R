#' Song file loading and basic preparation.
#' 
#' Opens wav file, also applies highpassfiltering and subsetting if required. 
#' Returns the \code{\link{Wave}} object.
#'
#' @param wavfile Name of wavfile (required).
#' @param path Path (optional, working directory if missing).
#' @param f Sampling rate (extracted from wavfile if NULL).
#' @param highpassfilter Frequency cutoff for high-pass filter (not applied if 
#'   NULL).
#' @param select Vector of 2 for selecting a specific time window (in seconds).
#' @param silenceAdd The number of samples that are added as silence at the 
#'   beginning and end of a file ('padding'). Sample units (not seconds!). No 
#'   silence is appended if silenceAdd=NULL.
#' 
#' @return 
#' Returns a the song as a \code{\link{Wave}} object.
#' 
#' @author Holger Schielzeth(holger.schielzeth@@uni-bielefeld.de).
#' 
#' @seealso 
#' \code{\link{curateStrophes}}, \code{\link{identifyStrophes}}, 
#' \code{\link{identifySyllables}}, \code{\link{measureSyllables}}, 
#' \code{\link{curateSyllables}}, \code{\link{loadSongfile}},
#' 
#' @examples   
#' \dontrun{
#' 
#' mysound = loadSongfile("myfile.wav", "C://mypath//", highpassfilter=1001, select=c(0.5,2))
#' }
#' 
#' @import tuneR
#' @import seewave
#' @importFrom signal butter
#' @importFrom signal filter
#' 
#' @export

loadSongfile <- function(wavfile, path="", f=NULL, highpassfilter=1001, select=NULL, silenceAdd=NULL) {
 	if(file.exists(paste0(path, wavfile))) {
	  song = tuneR::readWave(paste0(path, wavfile))
	} else stop(paste0("File ", path, wavfile, " does not exist."))
	if(is.null(f)) f=song@samp.rate #/2
	if(!is.null(select) && length(select)!=2 && select[1]<0 && select[2]<length(song)/f) stop ("select has to be within the limits of the duration of the .wav file.")
	if(!is.null(select)) song = seewave::cutw(song, from=select[1], to=select[2], output="Wave")
	if(!is.null(silenceAdd)) {
	  pastewave <- tuneR::silence(silenceAdd, xunit="samples", samp.rate=f, bit=song@bit, pcm=TRUE)
	  song = seewave::pastew(pastewave, song, output = "Wave")
	  song = seewave::pastew(song, pastewave, output = "Wave")
	}
	if(!is.null(highpassfilter)) {
	  #create a highpass filter with a cutoff at 1000 Hz
	  nyquist_freq <- f / 2
	  cutoff_freq <- highpassfilter
	  
	  #design highpass filter (Butterworth filter)
	  filter_coeffs <- butter(4, highpassfilter / (f/2), type = "high")
	  
	  #apply the filter to the song data and convert to numeric before applying filter
	  song_filtered_data <- signal::filter(filter_coeffs, song@left)
	  song_filtered_data <- as.numeric(song_filtered_data)
	  
	  #reconstruct the Wave object
	  song_filtered <- Wave(song_filtered_data, samp.rate = f, bit = song@bit, pcm = TRUE)
	}
	return(song_filtered)
}