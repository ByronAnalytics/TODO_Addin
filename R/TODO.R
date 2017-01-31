#' @title Build TODO list from package directory
#'
#' @description Addin scans all .R and .Rmd files in the poject directory for
#'   'TODO' comments TODO items are grouped by file name and ordered by line
#'   number, then writen to a seperate file: TODO.txt This file is overwritten
#'   each time the addin is executed.
#'
#' @param taxaList A vector of species names to be validated
#' @param dictionary A vector of valid species names
#' @param synTable A dataframe with current names paired with previous synonyms
#'
#' @return a dataframe of mis-matched names and their suggested taxonomy.
#' @export

buildTODO <- function(){
  DIR <- rstudioapi::getActiveProject()

  rFiles <- list.files(path = DIR, pattern = "(?i)\\.r$", recursive = TRUE)
  rmdFiles <- list.files(path = DIR, pattern = "(?i)\\.rmd$", recursive = TRUE)
  rFiles <- c(rFiles, rmdFiles)

  response <- tcltk::tkmessageBox(message = "Scan project directory for TODO Items (this may take a moment)?",
               icon = "question", type = "yesnocancel", default = "yes")
  if(tcltk::tclvalue(response) == "yes"){
    allTODOs <- lapply(rFiles, .GetTODOs, DIR = DIR)
    allTODOs<- do.call(c,allTODOs)

    cat(allTODOs, file="TODO.txt")

    msgBox <- tcltk::tkmessageBox(title = "ALERT!",
                                  message = "TODO List built!\n Would you like to open the file?'",
                                  icon = "question", type = "yesno")
    if(tcltk::tclvalue(msgBox) == "yes"){
      toOpen <- paste(DIR, "TODO.txt", sep = "/")
      if(file.exists(toOpen)){
        rstudioapi::navigateToFile(toOpen)
      } else {
        tcltk::tkmessageBox(message = "File wasn't found", icon = "error", type = "ok")
      }
    }
  } else {
    tcltk::tkmessageBox(message = "Addin was canceled", icon = "error", type = "ok")
  }

}


.GetTODOs <- function(FILE, DIR){
  # Scan R code as character vector, line by line.
  code <- scan(file=paste(DIR, FILE, sep = "\\"), what = "character", sep = "\n", blank.lines.skip = FALSE)

  # Id lines with a commented TODO statement (#TODO or # TODO), case insensitive
  hasTODO <- grepl("# ?\\bTODO\\b", x = code, ignore.case = TRUE)

  if(any(hasTODO)){
    todoList <- c(paste("TODO Items in:", FILE, '\n'), code[hasTODO])
    todoLines <- grep("# ?\\bTODO\\b", x = code, ignore.case = TRUE)
    for(i in 2:length(todoList)){
      # cleans out 'TODO' prefix, leaving only the task
      # TODO: identify individuals, group tasks by person.
      item <- todoList[i]
      item <- sub(pattern = "#", replacement = "", x = item)
      item <- sub(pattern = "\\bTODO\\b", replacement = "", x = item, ignore.case = TRUE)
      item <- sub(pattern = ":", replacement = "", x = item)
      item <- trimws(item)
      todoList[i] <- paste("  Line: ", todoLines[i-1], ") ", item, "\n", sep = "")
      #TODO: More testing
    }
  } else {
    warning("No TODO Items found")
    return(NULL)
  }
  return(todoList)
}
