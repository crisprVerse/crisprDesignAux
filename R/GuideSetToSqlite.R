#' @title Save GuideSet object as a SQLite database
#' 
#' @description Save GuideSet object as a SQLite database.
#' 
#' @param guideSet A \code{GuideSet} object from crisprDesign.
#' @param dbfile String specifying filename of the SQLite database.
#' @param useSpacerCoordinates Should the spacer coordinates be used
#'     as start and end coordinates? TRUE by default. If FALSE,
#'     the PAM site coordinate is used for both start and end. 
#' @param primaryOnly Should only the primary table (on-targets) be saved?
#'     FALSE by default.
#' @param overwrite Should data be overwritten if it exists?
#'     FALSE by default. 
#' 
#' @author Jean-Philippe Fortin
#' 
#' @examples 
#' library(crisprDesign)
#' data(guideSetExampleFullAnnotation, package="crisprDesign")
#' GuideSetToSqlite(guideSetExampleFullAnnotation,
#'                  tempfile())
#' 
#' @export 
#' @importFrom RSQLite SQLite dbWriteTable
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom crisprDesign GuideSet2DataFrames
GuideSetToSqlite <- function(guideSet,
                             dbfile=NULL,
                             useSpacerCoordinates=TRUE,
                             primaryOnly=FALSE,
                             overwrite=FALSE
){
    tabs <- GuideSet2DataFrames(guideSet,
                                useSpacerCoordinates=useSpacerCoordinates,
                                primaryOnly=primaryOnly)
    # Need to change primary to primaryTable as it is
    # a reserved word in SQlite:
    wh <- which(names(tabs)=="primary")
    names(tabs)[wh] <- "primaryTable"
    
    conn <- dbConnect(RSQLite::SQLite(), dbfile)
    on.exit(dbDisconnect(conn))
    for (tab in names(tabs)){
        dbWriteTable(conn,
                     tab,
                     tabs[[tab]],
                     overwrite=overwrite)
    }
    invisible(TRUE)
}


