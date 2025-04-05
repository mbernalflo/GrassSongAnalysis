#' Function for curating automatically detected strophe cutoffs.
#' 
#' The function takes existing cutoffs (typically read from a textfile), plots 
#' the amplitude envelpe and allows interactive setting of different cutoffs. 
#' NOTE: The function will only return the x-location (sample) of the new 
#' cutoff. The textfile will open, but it will have to be changed manually. 
#' Don't forget to save. The function will end with redrawing the .bmp file 
#' with the new cutoffs.
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

curateStrophes = function(envFine, wavfile, stropath, stroCutoffs=NULL, f=44100, wl=44, ovlp=50, padding=TRUE, 
                          rollmeanStroWW=c(11, 101), StroThreshExceedWW=0, StroThreshNeeded=0, stroAmpThresh=15, MinStroDur=0, 
                          confirmRevised = TRUE, replotToFile = TRUE, closeAllWindowsOnStart=TRUE, manualEdit = TRUE, 
                          bmpwidth=3200, bmpheight=600, forceStartAtZero = TRUE, ...) {
  # Initial checks
  if(is.logical(padding) && padding) padding = StroThreshExceedWW - StroThreshNeeded + 1
  if(is.logical(padding) && padding ==FALSE || is.null(padding)) padding = 0
  if(closeAllWindowsOnStart) replicate(length(dev.list()), dev.off())

  updateTable = function(newCutoffs, stroCutoffs) {
    newStartSampWithPad = newCutoffs[seq(1,length(newCutoffs), by=2)]
    newEndSampWithPad   = newCutoffs[seq(2,length(newCutoffs), by=2)]
    lenDiff = length(newStartSampWithPad)-nrow(stroCutoffs)
    if(lenDiff>0) for(i in 1:lenDiff) stroCutoffs = rbind(stroCutoffs, rep(NA, ncol(stroCutoffs)))
    if(lenDiff<0) stroCutoffs = stroCutoffs[1:length(newStartSampWithPad),]
    stroCutoffs$StartSampWithPad = newStartSampWithPad
    stroCutoffs$EndSampWithPad   = newEndSampWithPad
    stroCutoffs$StroValid        = 1
    stroCutoffs$StroCounter      = 1:nrow(stroCutoffs)
    stroCutoffs[,4:8] = NA
    stroCutoffs = recalcCutoffNAs(stroCutoffs, f=f, wl=wl, ovlp=ovlp, padding=padding, file=paste0(stropath, unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],".txt"))
    return(stroCutoffs)
  }
      
  if(manualEdit) {
    dev.new(width = 30, height = 7)
    xlim=NA
    stroSel=NA
    repeat{
      identifyStrophes(envFine, wavfile=wavfile, stropath=stropath, toFile=FALSE, 
           wl=wl, ovlp=ovlp, padding=padding, MinStroDur=MinStroDur,
           StroThreshExceedWW=StroThreshExceedWW, StroThreshNeeded=StroThreshNeeded, stroAmpThresh=stroAmpThresh,
           stroCutoffs=stroCutoffs, rollmeanStroWW=rollmeanStroWW, writeRes=FALSE, xlim=xlim, ...)
      print("Locate new thresholds in plot ...")
      newCutoffs <- locator()$x
      if(is.null(newCutoffs)) {
        print(paste0("No changes to file ", wavfile, "."))
      }
      else {
        if(length(newCutoffs) %% 2 == 1) {
          print(paste0("An uneven number of  cuttoff points was selected! Please select and even number"))
          newCutoffs = newCutoffs[-length(newCutoffs)]
        }
        newCutoffs = round(newCutoffs,0)
        print(newCutoffs)
        if(!is.na(stroSel) && !is.null(newCutoffs)) {
          newStartSampWithPad = newCutoffs[seq(1, length(newCutoffs), 
                                            by = 2)]
          newEndSampWithPad = newCutoffs[seq(2, length(newCutoffs), 
                                             by = 2)]
          stroCutoffs = stroCutoffs[-stroSel,]  
          for (i in 1:length(newStartSampWithPad)) stroCutoffs = rbind(stroCutoffs, 
                                                                       c(NA, newStartSampWithPad[i], newEndSampWithPad[i], 
                                                                         rep(NA, ncol(stroCutoffs) - 4), 1))
          stroCutoffs = stroCutoffs[order(stroCutoffs$StartSampWithPad), 
                                  ]
          stroCutoffs$StroCounter = 1:nrow(stroCutoffs)
          stroCutoffs[, 4:8] = NA
          stroCutoffs = recalcCutoffNAs(stroCutoffs, f = f, wl = wl, 
                                      ovlp = ovlp, padding = padding, file = paste0(stropath, 
                                                                                    unlist(strsplit(wavfile, split = ".", fixed = TRUE))[1], 
                                                                                    ".txt"))
          abline(v = newCutoffs, col = "green", lwd = 2)
          newCutoffs=NULL
        }
        abline(v=newCutoffs, col="green", lwd=2)
      }
      print("Choose: [D] Done, [C] Cancel & repeat, [S] save & change more, [E] edit file (cancel changes), [1-20] Zoom to strophe. Waiting for input ...")
      answer = readline()
      xlim=NA
      stroSel=NA
      if(answer=="S" | answer=="s") {
        if(!is.null(newCutoffs)) stroCutoffs = updateTable(newCutoffs, stroCutoffs)
      }
      if(answer=="E" | answer=="e") {
        file.edit(paste0(stropath, unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],".txt"))
        print("Modify file and press RETURN when done...")
        scan()
        stroCutoffs = read.table(paste0(stropath, unlist(strsplit(wavfile, split='.', fixed=TRUE))[1],".txt"), header=TRUE, sep="\t")
      }
      if(answer=="1" || answer=="2" || answer=="3" || answer=="4" || answer=="5" || answer=="6" || answer=="7" || answer=="8" || answer=="9" || answer=="10" || 
         answer=="11" || answer=="12" || answer=="13" || answer=="14" || answer=="15" || answer=="16" || answer=="17" || answer=="18" || answer=="19" || answer=="20") {
        if(as.numeric(answer)>nrow(stroCutoffs)) print("Input ignored, because number larger than the number of strophes in file.")
        else xlim = c(stroCutoffs$StartSampWithPad[as.numeric(answer)]-300, stroCutoffs$EndSampWithPad[as.numeric(answer)]+300)
        stroSel = as.numeric(answer)
      }
      if(answer=="D" | answer=="d" | answer=="") {
        if(!is.null(newCutoffs)) stroCutoffs = updateTable(newCutoffs, stroCutoffs)
        break
      }
    }
    dev.off()
  }
  if(forceStartAtZero && any(stroCutoffs$StartSampWoutPad<0))  {
    stroCutoffs$StartSampWithPad[stroCutoffs$StartSampWithPad<padding] = padding
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
