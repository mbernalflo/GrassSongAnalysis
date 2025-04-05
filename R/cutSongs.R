#'Cuts a .wav file into smaller .wav files using a .txt file as reference.
#'
#'
#'\code{\link{cutSongs}} cuts a bigger \code{.wav} file into smaller \code{.wav} files each containing  a song identified by  \code{\link{identifyStrophes}} and \code{\link{mergePhrases}}.
#' The new files are numbered sequentially (namefile_song01.wav).
#'
#' 
#' 
#' @param tablepath path to a txt file written like this: \code{"./namefile.txt"}.
#' @param song wave object previously loaded using \code{\link{loadSongfile}}
#' @param songs_folder directory where the songs should be written into
#' @param buffer adds time (in seconds) before and after the identified song when cutting the files. (Suggested max a couple of seconds, default is 0.5s)
#' @param overwriteRes Boolean. This is just a safety catch. If there already is a txt file with the same name and \code{overwriteRes = FALSE} the function will stop in order not to overwrite previous manual edits. Default is FALSE.
#' 
#' 
#' @details
#' The algorithm in brief:
#' \itemize{
#'  \item Read in the table and create empty list
#'  \item Initial wave object goes through a loop to cut out only valid songs
#'  \item Checks if the file already exists
#'  \item Creates new .wav file for each song
#' }
#' 
#' @return 
#' Nothing
#' 
#' @note 
#' Songs with StroValid=0 will not be considered.
#' 
#' @author Mar Bernal Flo  (mar.bernal.flo@@uni-jena.de)
#'  
#' @seealso 
#' \code{\link{loadSongfile}}, \code{\link{curateStrophes}}, 
#' \code{\link{identifySyllables}}, \code{\link{measureSyllables}}, 
#' \code{\link{identifyPulses}}, \code{\link{curateSyllables}},
#' \code{\link{identifyStrophes}}, \code{\link{mergePhrases}}
#' 
#' @examples   
#' 
#' \dontrun{
#' 
#' tablepath <- "./ZMC008_240801_1635_test2.txt"
#' song <- loadSongfile(
#' "ZMC008_240801_1635_test3.wav",
#' #  path = "~/Desktop/Università/UNIJENA/MASTER #THESIS/song recordings/clusterA/2024-08-01/",
#' f = NULL,
#' highpassfilter = 3000,
#' select = NULL,
#' silenceAdd = NULL)
#' buffer <- 1
#' cutSongs(tablepath, song, songs_folder, buffer, envFine = envFine)
#' }
#' 
#' @import tuneR
#' 
#' @export

cutSongs <- function(
    tablepath,
    song, 
    songs_folder="",
    buffer = 0.5,
    overwriteRes = FALSE
){
  
  if(songs_folder!="") {
    if (!dir.exists(songs_folder)) {
      dir.create(songs_folder)
      print(paste0("Directory ", songs_folder, " did not exist and was created."))
    }
  }
  
  #import the data from the table
  cuts <- read.table(tablepath, header = TRUE)
  #create empty list to store wave segments
  song_segments <- list()
  
  #loop to cut segments adding a buffer
  for (i in 1:nrow(cuts)) {
    if(cuts$StroValid[i]==1){
      start_sample <- max(0, (cuts$StartSec[i] - buffer))
      end_sample <- min(length(song),(cuts$EndSec[i] + buffer))
      segment <- extractWave(song, from = start_sample, to = end_sample, xunit = "time")
      segment@left <- as.integer(segment@left)
      #append valid segment to the list
      song_segments[[length(song_segments) + 1]] <- segment
    }
  }
  
  #basename for the new files
  tablename <- tools::file_path_sans_ext(basename(tablepath))
  
  #export segments into wav files with correct numbering
  for (i in seq_along(song_segments)) {
    file_name <- sprintf("%s_song%02d.wav", tablename, i)
    write_path <- file.path(songs_folder, file_name)
    #safety check if there is already a file with that name
    if(file.exists(write_path) && overwriteRes==FALSE){
      print("File not written, because it already exists! Set overwriteRes = TRUE if you are sure you want to replace the existing file!")
    } 
    else{
      writeWave(song_segments[[i]], write_path)
    }  
  }
  return(song_segments)
}
