#' Extract measurements for syllables or pulses.
#' 
#' Exacts the start, end, max, min and mean amplitute for each syllable/pulse.
#' @param envFine A sound envelope as a vector of numbers.
#' @param wavfile Name of wavfile (necessary).
#' @param syllpath Path to folder in wich bmp files should be plotted 
#'   (optional, working directory if missing).
#' @param syllCutoffs TO BE ADDED
#' @param useResFile Boolean. A check is done, if a results file already exists and
#'   the results of this file are used in \code{useResFile = TRUE}
#' 
#' @return 
#' Returns a \code{data.frame}, which is \code{Cutoffs} augmented with five 
#' columns of measurements.
#' 
#' @references 
#' No references.
#' 
#' @author Holger Schielzeth  (holger.schielzeth@@uni-bielefeld.de).
#' 
#' @seealso 
#' \code{\link{loadSongfile}}, \code{\link{curateStrophes}}, 
#' \code{\link{identifyStrophes}}, \code{\link{identifySyllables}}, 
#' \code{\link{measureSyllables}}, \code{\link{curateSyllables}}
#' 
#' @examples   
#'
#' \dontrun{
#' 
#' mysound = filePreparation("myfile.wav", "C://mypath//", 
#    highpassfilter=1001, select=c(0.5,2))
#' myenv = plotEnvolopes(mysound, "myfile.wav", "c://mystropath//",
#'   padding = 300)
#' mystro = plotStrophes(myenv$envFine, "myfile.wav", "C://mypath//", 
#'   measure=TRUE)
#' }
#' 
#' @export

measureSyllables <- function(song, envFine, wavfile, syllpath, syllCutoffs=NULL, useResFile=TRUE) {
                          # rollmeans should be odd numbers! Add check for this!
  allsyllfiles  = dir(syllpath)
  wavfilesig    = sub(pattern=".wav", replace="", x=wavfile)
  syllfiles     = grep(pattern=wavfilesig, x=allsyllfiles, ignore.case=TRUE, value=TRUE)
  syllfiles     = grep(pattern="_Syllables.txt", x=syllfiles, ignore.case=TRUE, value=TRUE)
  if(length(syllfiles)>0) {
    for (j in 1:length(syllfiles)){
      print(paste0("Measure syllables: ", syllfiles[j]))
      # rollmeans should be odd numbers! Add check for this!
      resFile = paste0(syllpath, sub("s.txt", "", syllfiles[j]),"Data.csv")
      if(useResFile && file.exists(resFile)) {
        syllMeas = NULL
        print("Syllable measurements are already available. Set useResFile=FALSE if you want to recalculate them.")
      }
      else {
        syllCutoffs = read.table(paste0(syllpath, syllfiles[j]),header=TRUE)
        syllMeas = measureAnything(song, envFine, syllCutoffs, type="Syll")
        write.table(syllMeas, resFile, sep=",", quote=FALSE, row.names=FALSE)  # Return values
      }
    }
  }
  return(syllMeas)	
}