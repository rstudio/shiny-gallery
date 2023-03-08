#' Function to read structural files or NGL session
#' 
#' @description    
#' Function to read .PDB or .NGL files
#'
#' @param file The file input. Either a PDB code or structural file.
#' 
#' @importFrom tools file_ext
#' @importFrom readr read_lines read_csv
#' @export
readFile <- function(file) {
  File <- list(
    PDB = NULL,
    structure = NULL,
    surface = NULL,
    ligand = NULL,
    stage = NULL,
    selections = NULL,
    labels = NULL,
    contacts = NULL,
    fileExt = NULL
  )
  
  #If code is entered
  fileExt <- tools::file_ext(file)
  if(nchar(file) < 8 && tools::file_ext(file) == ""){
    File$PDB <- file
    return(File)
  }
  
  #If file is not NGL format
  if (fileExt != "ngl" && fileExt != "NGL") {
    File$PDB <- file
    File$fileExt <- fileExt
    return(File)
  }
  
  NGL <- readr::read_lines(file)
  File$fileExt <- "pdb"
  File$PDB <- NGL
  
  # Get file start
  structure_start <- grep(pattern = "#STRUCTURE", x = NGL)
  surface_start <- grep(pattern = "#SURFACE", x = NGL)
  ligand_start <- grep(pattern = "#LIGAND", x = NGL)
  sel_start <- grep(pattern = "#SELECTIONS", x = NGL)
  
  labels_start <- grep(pattern = "#LABELS", x = NGL)
  contacts_start <- grep(pattern = "#CONTACTS", x = NGL)
  stage_start <- grep(pattern = "#STAGE", x = NGL)
  PDB_start <- grep(pattern = "#PDB", x = NGL)
  
  # load data frames
  if(surface_start - structure_start > 1){
  File$structure <- data.frame(readr::read_csv(NGL, skip = structure_start, n_max = surface_start - (structure_start + 2)))
  }
  if(ligand_start - surface_start > 1){
  File$surface <- data.frame(readr::read_csv(NGL, skip = surface_start, n_max = ligand_start - (surface_start + 2)))
  }
  if(stage_start - ligand_start > 1){
  File$ligand <- data.frame(readr::read_csv(NGL, skip = ligand_start, n_max = stage_start - (ligand_start + 2)))
  }
  if(sel_start - stage_start > 1){
  File$stage <- data.frame(readr::read_csv(NGL, skip = stage_start, n_max = sel_start - (stage_start + 2)))
  }
  if ((labels_start - (sel_start +1)) > 1) {
    File$selections <- data.frame(readr::read_csv(NGL, skip = sel_start, n_max = labels_start - (sel_start + 2)))
  }
  if ((contacts_start - (labels_start + 1)) > 1) {
    File$labels <- data.frame(readr::read_csv(NGL, skip = labels_start, n_max = contacts_start - (labels_start + 2)))
  }
  if ((PDB_start - (contacts_start + 1)) > 1) {
    File$contacts <- data.frame(readr::read_csv(NGL, skip = contacts_start, n_max = PDB_start - (contacts_start + 2)))
  }
  return(File)
}