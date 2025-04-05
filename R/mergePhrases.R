#' Merges phrases into the whole song when mistakenly identified by \code{\link{identifyStrophes}}.
#' 
#' Merges phrases, obtained with \code{\link{identifyStrophes}}, that are separated by less than \code{maxsilence} (in seconds) to encompass a full song. 
#' Creates a new \code{.txt} file, inside the working directory, with the name of the input table, to overwrite set \code{overwriteRes=TRUE}.
#' The new file contains a table with the same columns as the one created by \code{\link{identifyStrophes}}, updated \code{StroCounter} and \code{StroValid}.
#' 
#' @param tablepath path to a txt file written like this: \code{"./namefile.txt"}.
#' @param maxsilence maximum length, in seconds, of the distance between sound to be considered from the same song.
#' @param minSonglength the minimum length, in seconds, to determine if the resulting sound is a song or not. Result of this check will be written in \code{StroValid}, with 0 if not valid and 1 if valid.
#' @param envFine A sound envelope as a vector of numbers.
#' @param bmpwidth Width of the tiff image (in pixels).
#' @param bmpheight Height of the tiff image (in pixels).
#' @param wl Window length for the (finest) sound envelope given as the number of sample points, At f=44100: 44 eq 1 ms (recommended), 441 eq 10 ms, 4410 eq 100 ms. Default is 44.
#' @param ovlp overlap between two successive windows (in percent, i.e. 50). Default is 50.
#' @param rollmeanStroWW One number, default is 101. The rolling mean window width for determining the intermediate an coarse envelopes from the finest resolution envelope determined by \code{wl} and \code{ovlp}. The code works more accurate for odd numbers!
#' @param stroAmpThresh Envelope threshold for identification of silence/sound (i.e., Strophes). Recommended value depends on background noise level. Default is 0.01.
#' @param f Sampling rate (if not known 44100 is used).
#' @param overwriteRes Boolean. This is just a safety catch. If there already is a txt file with the same name and \code{overwriteRes = FALSE} the function will stop in order not to overwrite previous manual edits. Default is FALSE.
#' @param plot Boolean, whether or not a plot should be in created. Default is TRUE.
#' @param toFile Boolean, whether or not the plot should be in a .tiff file (if TRUE) or in a plotting window (if FALSE). Default is TRUE.
#' @param xlim Vector of two numbers used to set x-limits in plot. Useful only to zoom in when cutoffs are know. Default is NULL.
#' 
#' @details
#' The algorithm in brief:
#' \itemize{
#'  \item Read in the table and do initial checks
#'  \item Checks if the distance between the identified sound is shorter than \code{maxsilence} and if yes it creates one row on the dataframe containing the start of the first one, the end of the last one of these segments and the duration of the new sound.
#'  \item Checks if the new sound is longer than \code{minSonglength} and evaluates if it is or is not a song. Result is found in the \code{StroValid} column of the new dataframe.
#'  \item Replots the results as a .tiff image.
#'  \item Rewrites a new .txt file containing the new dataframe.
#' }
#' 
#' @return 
#' Dataframe with the new cut off points
#' 
#' @note 
#' If the input table has less than 2 rows it stops and returns NULL.
#' 
#' @author Mar Bernal Flo  (mar.bernal.flo@@uni-jena.de)
#'  
#' @seealso 
#' \code{\link{loadSongfile}}, \code{\link{curateStrophes}}, 
#' \code{\link{identifySyllables}}, \code{\link{measureSyllables}}, 
#' \code{\link{identifyPulses}}, \code{\link{curateSyllables}},
#' \code{\link{identifyStrophes}}, \code{\link{env}}
#' 
#' @examples   
#' 
#' \dontrun{
#' 
#' tablepath <- "./ZMC008_240801_1635_test2.txt"
#' maxsilence <- 2.2 
#' minSonglength <- 10
#' 
#' mergePhrases(tablepath, maxsilence, minSonglength, envFine = envFine)
#' }
#' 
#' @export
#' 
#' 
#function definition
mergePhrases <- function(
    tablepath, 
    maxsilence, 
    minSonglength,
    envFine,
    bmpwidth = 1200,
    bmpheight = 600,
    wl = 44,
    ovlp = 50,
    rollmeanStroWW = 101,
    stroAmpThresh=0.01,
    f = 44100,
    overwriteRes = FALSE,
    plot = TRUE,
    toFile = TRUE,
    xlim = NA
){
  #read in the txt file
  table <- read.table(tablepath, header = TRUE)
  
  #check if the table has more than 1 row
  if (nrow(table) <= 1) {
    message("Table must have more than 1 row to proceed.")
    return(NULL) # Stop the function and return NULL
  }
  
  #create a new dataframe containing the same columns as the table created by identifyStrophes()
  df <- data.frame(matrix(ncol = 9, nrow = 0))
  colnames(df) <- colnames(table)
  
  #initialize parameters needed for the for loop
  count <- nrow(table)
  start_index <- 1
  
  for (i in 1:(count-1)) {
    # Check if the difference condition holds between the current row and the next row
    if (!is.na(table$StartSec[i + 1]) && !is.na(table$EndSec[i]) && 
        (table$StartSec[i + 1] - table$EndSec[i]) <= maxsilence) {
      # If condition is met, continue checking the next row
      next
    } else {
      # If condition fails, check if we've tracked a valid range
      if (i > start_index) {
        # Create a new row using the first (start_index) and last (i) rows in the range
        new_row <- data.frame(
          StroCounter = NA,
          StartSampWithPad = table[start_index, 2],
          EndSampWithPad = table[i, 3],
          StartSampWoutPad = table[start_index, 4],
          EndSampWoutPad = table[i, 5],
          StartSec = table[start_index, 6],
          EndSec = table[i, 7],
          DurSec = (table[i, 7] - table[start_index, 6]),
          StroValid = NA
        )
        
        # Append the new row to df
        df <- rbind(df, new_row)
      }
      
      # Reset start_index to the next row after a gap is found
      start_index <- i + 1
    }
  }
  
  # After the loop, check if there is still a range that meets the condition, or if all the rows meet the condition
  if (start_index <= count) {
    new_row <- data.frame(
      StroCounter = NA,
      StartSampWithPad = table[start_index, 2],
      EndSampWithPad = table[count, 3],
      StartSampWoutPad = table[start_index, 4],
      EndSampWoutPad = table[count, 5],
      StartSec = table[start_index, 6],
      EndSec = table[count, 7],
      DurSec = (table[count, 7] - table[start_index, 6]),
      StroValid = NA
    )
    
    # Append the last range row to df
    df <- rbind(df, new_row)
  }
  
  #determine if songs are valid songs or not
  for(i in 1:nrow(df)){
    if(df$DurSec[i] < minSonglength){
      df$StroValid[i] <- 0
    }else{
      df$StroValid[i] <- 1
    }
  }
  
  # Actualize the counter
  df$StroCounter <- seq_len(nrow(df))
  
  
  
  ##### Envelope identification ####
  
  envStro = zoo::rollmean(envFine, rollmeanStroWW, align="center")
  envStro = c(rep(envStro[1], (rollmeanStroWW-1)/2), envStro, rep(envStro[length(envStro)], (rollmeanStroWW-1)/2))
  mytime = length(envFine)/(f/(wl*(1-ovlp/100)))
  
  ######### plot ########
  
  if(plot){
    if(toFile){
      tiff(
        filename = sub("\\.txt$", ".tiff", tablepath),
        width = bmpwidth, 
        height = bmpheight
      )
      on.exit(dev.off())
    }
    if(any(is.na(xlim))) {
      plot(envFine, type="n", xaxt="n", xlab="Time (sec)", col="red")
    }
    if(any(!is.na(xlim))) {
      plot(envFine, type="n", xaxt="n", xlab="Time (sec)", col="red", xlim=xlim)
    }
    text(df$StartSampWithPad, max(envFine)*0.90, labels=df$StroCounter, pos=4, cex=8, col=c("grey90", "red")[df$StroValid + 1])
    points(envFine, type="l", col="red")
    points(envStro, type="l", col="black")
    abline(h=stroAmpThresh)
    axis(1, at=seq(0, mytime, by=0.5) * (f / (wl * (1 - ovlp / 100))), labels=seq(0, mytime, by=0.5))
    abline(v=df$StartSampWithPad, col="black", lwd=c(1, 2)[df$StroValid + 1], lty=1) 
    abline(v=df$EndSampWithPad, col="black", lwd=c(1, 2)[df$StroValid + 1], lty=1) 
    
  }
  
  ####### write dataframe into file #######
  
  # #rewrite txt file with new dataframe
  # newname <- sub("\\.txt$", "_rw.txt", tablepath)
  
  #safety check if there is already a file with that name
  if(file.exists(tablepath) && overwriteRes==FALSE){
    print("File not written, because it already exists! Set overwriteRes = TRUE if you are sure you want to replace the existing file!")
  } 
  else{
    write.table(df, file = tablepath, sep = "\t", row.names = FALSE, quote = TRUE)
  }
  
  return(df)
  
}




# write.table(df, file = basename(tablepath), sep = "\t", row.names = FALSE, quote = TRUE)