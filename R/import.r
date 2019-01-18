#' Import GMT File
#'
#' Imports a `.gmt` format file and produces a `BaseSets` class object. 
#' A `.gmt` file is a non-rectangular tab-separated format, where each row 
#' delimits a unique geneset. The first column is the geneset name, the second 
#' column is the geneset source (such as a URL), and the third columns onwards 
#' contain the names of the geneset constituents, such that each geneset can 
#' have a variable length.
#'
#' @param path a file name or URL containing gene sets.
#' 
#' @return Returns a `BaseSets` object with slot `setData` containing set 
#'   source information.
#'
#' @author Robert Amezquita, adapted from Kayla Morrell GeneSe pkg
#'
#' @rdname import
#' @export
#' @importFrom S4Vectors DataFrame
#' @importFrom utils stack
#' @examples
#' gmt_file <- system.file(package = "unisets", "extdata",
#'     "example.gmt")
#' gmt_BaseSet <- importGMT(gmtFile)

importGMT <- function(path) {
    ## Read in GMT into a list format
    sets <- readLines(path)
    sets <- strsplit(sets, "\t")
    names <- vapply(sets, function(set) set[[1]], character(1))
    genes <- lapply(sets, function(set) set[-(1:2)])
    names(genes) <- names

    ## Produce an error if names contains duplicates
    if (length(unique(names)) != length(names)) {
        dups <- names[duplicated(names)]
        err <- paste0("Duplicated geneset names exist for the sets below. ",
            "Please check your GMT file.\n\n", 
            c(paste0(dups, collapse = '\n')), '\n\n')
        stop(err)
    }

    ## Convert GMT to DataFrame of element:set
    map <- DataFrame(stack(genes))
    colnames(map) <- c("element", "set")
    map$set <- as.character(map$set)

    ## Extract GMT source (url) for elementData slot
    source <- vapply(sets, function(set) set[[2]], character(1))
    source[source=="NA" | !nzchar(source)] <- NA
    source <- DataFrame(
        row.names = names,
        source = source)

    return(BaseSets(map, setData = source))
}