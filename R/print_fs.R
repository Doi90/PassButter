#' @title print_fs
#'
#' @description Print a file structure to the console. Akin to the Unix 'tree' command
#'
#' @usage print_fs(path = ".", depth = 2L)
#'
#' @param path file path to location where you want to visualise the file structure
#'
#' @param depth an integer to set how deep you want to visualise the file structure
#'
#' @author David Wilkinson \email{davidpw@student.unimelb.edu.au}
#'
#' @section Date submitted: 2019-03-26
#'
#' @section Last Modified: 2019-03-26
#'
#' @examples print_fs(depth = 2)
#'
#' @return Prints file structure to console
#'
#' @export

## Credit: STATWORX
## https://github.com/STATWORX/helfRlein

print_fs <- function(path = ".",
                     depth = 2L) {

  ## Check path

  if(length(path) != 1){
    stop("Path must have length one")
  }
  if(!is.character(path)){
    stop("Path must be a character")
  }
  if(!dir.exists(path)){
    stop("Path does not exist")
  }

  ## Check depth

  if(!is.numeric(depth)){
    stop("Depth must be a positive integer")
  }
  if(!is.integer(depth)){
    depth <- ceiling(depth)
  }
  if(depth <= 0){
    warning("Depth was negative, set to 2")
    depth <- 2L
  }

  ## Get files

  files <- list.files(path = path,
                      recursive = TRUE,
                      include.dirs = FALSE)

  ## Transform to data.tree

  df <- data.frame(filename = paste0(basename(path), "/", files))

  file_structure <- data.tree::as.Node(df, pathName = "filename")

  ## Pruning

  file_structure$Do(function(node) node$depths <- min(node$Get("level")))

  data.tree::Prune(file_structure, function(node)  node$depths <= depth)

  ## Return value

  return(file_structure)

}
