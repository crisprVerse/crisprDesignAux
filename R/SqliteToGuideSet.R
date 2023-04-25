#' @title Retrieve a GuideSet object from a SQLite database
#' 
#' @description Save GuideSet object as a SQLite database.
#' 
#' @param dbfile String specifying filename of the database.
#' @param crisprNuclease \code{CrisprNuclease} object from the 
#'     crisprBase package representing the nuclease used to 
#'     build the database.
#' @param bsgenome \code{BSgenome} object representing the genome
#'     used to build the database. 
#' @param targetOrigin String specifying how the GuideSet was 
#'     constructed. Must be either "bsgenome" or "customSequences".
#' @param customSequences Optional DNA sequences used to build the 
#'     underlying GuideSet objects. Must only be provided when 
#'     targetOrigin is set to "customSequences".
#' @param id.field String specifying the name of the column
#'    used to store gRNA ids.
#' @param ... Other arguments to pass to GuideSet constructor function.
#' @author Jean-Philippe Fortin
#' 
#' @examples 
#' library(crisprDesign)
#' data(guideSetExampleFullAnnotation, package="crisprDesign")
#' dbfile <- tempfile()
#' GuideSetToSqlite(guideSetExampleFullAnnotation, dbfile)
#' 
#' library(crisprBase)
#' library(BSgenome.Hsapiens.UCSC.hg38)
#' data(SpCas9)
#' crisprNuclease <- SpCas9
#' bsgenome <- BSgenome.Hsapiens.UCSC.hg38
#' gs <- SqliteToGuideSet(dbfile,
#'                        bsgenome=bsgenome,
#'                        crisprNuclease=crisprNuclease,
#'                        seqinfo=seqinfo(bsgenome),
#'                        seqlengths=seqlengths(bsgenome))
#' 
#' @export 
#' @importFrom RSQLite SQLite dbWriteTable
#' @importFrom DBI dbConnect
#' @importFrom methods as validObject
#' @importFrom GenomeInfoDb genome genome<-
SqliteToGuideSet <- function(dbfile,
                             crisprNuclease=NULL,
                             bsgenome=NULL,
                             targetOrigin=c("bsgenome", "customSequences"),
                             customSequences=NULL,
                             id.field="ID",
                             ...
){  
    targetOrigin <- match.arg(targetOrigin)
    tables <- .retrieveGuideSetTables(dbfile)
    gs <- .buildCoreGuideSet(tables,
                             crisprNuclease,
                             bsgenome=bsgenome,
                             customSequences=customSequences,
                             targetOrigin=targetOrigin,
                             ...)
    names(gs) <- mcols(gs)[[id.field]]
    ids <- unique(names(gs))
    gs <- .addSecondaryTables(gs, tables, ids, id.field=id.field)
    gs <- as(gs, "GuideSet")
    if (targetOrigin=="bsgenome"){
        genome(gs) <- genome(bsgenome)
    }
    validObject(gs)
    return(gs)
}





#' @importFrom DBI dbExistsTable
#' @importFrom DBI dbReadTable dbDisconnect
.retrieveGuideSetTables <- function(dbfile){
    conn <- dbConnect(RSQLite::SQLite(), dbfile)
    on.exit(dbDisconnect(conn))
    
    cols <- c("primaryTable",
              "alignments",
              "geneAnnotation",
              "tssAnnotation",
              "enzymeAnnotation",
              "snps")
    exists <- vapply(cols, function(col){
        dbExistsTable(conn, col)
    }, FUN.VALUE=TRUE)
    cols <- cols[exists]
    if (!"primaryTable" %in% cols){
        stop("primaryTable table must be in SQLite database.")
    }
    tables <- lapply(cols, function(col) dbReadTable(conn, col))
    names(tables) <- cols
    return(tables)
}





#' @importFrom crisprDesign GuideSet
#' @importFrom Biostrings DNAStringSet
#' @importFrom S4Vectors mcols mcols<-
.buildCoreGuideSet <- function(tables,
                               crisprNuclease,
                               bsgenome=NULL,
                               customSequences=NULL,
                               targetOrigin=c("bsgenome",
                                              "customSequences"),
                               id.field="ID",
                               ...
){
    targetOrigin <- match.arg(targetOrigin)
    primary <- tables[["primaryTable"]]
    if ("chr" %in% colnames(primary)){
        seqcol <- "chr"
    } else if ("seqnames" %in% colnames(primary)){
        seqcol <- "seqnames"
    } else {
        stop("Cannot find chr or seqnames as column in primary table.")
    }
    gs <- GuideSet(ids=primary[[id.field]],
                   protospacers=primary[["protospacer"]],
                   pams=primary[["pam"]],
                   seqnames=primary[[seqcol]],
                   pam_site=primary[["pam_site"]],
                   strand=primary[["strand"]],
                   CrisprNuclease=crisprNuclease,
                   bsgenome=bsgenome,
                   customSequences=customSequences,
                   targetOrigin=targetOrigin,
                   ...)
    colsSup <- setdiff(names(primary),colnames(mcols(gs)))
    coreCols <- c("chr", "seqnames", "start", "end","strand")
    colsSups <- setdiff(colsSup, coreCols)
    if (length(colsSups)>0){
        for (col in colsSups){
            mcols(gs)[[col]] <- primary[[col]]
        }
    }
    return(gs)
}





#' @importFrom crisprDesign alignments<-
#' @importFrom crisprDesign geneAnnotation<-
#' @importFrom crisprDesign tssAnnotation<-
#' @importFrom crisprDesign snps<-
#' @importFrom crisprDesign enzymeAnnotation<-
.addSecondaryTables <- function(guideSet,
                                tables,
                                ids,
                                id.field="ID"
){
    tables <- tables[setdiff(names(tables), "primaryTable")]
    cols <- names(tables)
    if ("alignments" %in% cols){
        alignments(guideSet) <- .formatAlignments(tables[["alignments"]],
                                                  ids=ids,
                                                  id.field=id.field)
    }
    if ("geneAnnotation" %in% cols){
        geneAnnotation(guideSet) <- .formatGeneAnnotation(tables[["geneAnnotation"]],
                                                          ids=ids,
                                                          id.field=id.field)
    }
    if ("tssAnnotation" %in% cols){
        tssAnnotation(guideSet) <- .formatTssAnnotation(tables[["tssAnnotation"]],
                                                        ids=ids,
                                                        id.field=id.field)   
    }
    if ("enzymeAnnotation" %in% cols){
        enzymeAnnotation(guideSet) <- .formatEnzymeAnnotation(tables[["enzymeAnnotation"]],
                                                              ids=ids,
                                                              id.field=id.field)   
    }
    if ("snps" %in% cols){
        snps(guideSet) <- .formatSnps(tables[["snps"]],
                                      ids=ids,
                                      id.field=id.field)
    }
    return(guideSet)
}





#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom S4Vectors mcols<- 
#' @importFrom Biostrings DNAStringSet
.formatAlignments <- function(alignments,
                              ids=NULL,
                              id.field="ID"
){
    if ("chr" %in% colnames(alignments)){
        seqcol <- "chr"
    } else if ("seqnames" %in% colnames(alignments)){
        seqcol <- "seqnames"
    } else {
        stop("Cannot find chr or seqnames as column in alignments")
    }
    gr <- GRanges(alignments[[seqcol]],
                  IRanges(start=alignments$start,
                          end=alignments$end),
                  strand=alignments$strand)
    cols <- colnames(alignments)
    cols <- setdiff(cols, c("chr","seqnames", "strand", "start", "end"))
    mcols(gr) <- alignments[,cols]
    mcols(gr)$spacer <- DNAStringSet(mcols(gr)$spacer)
    mcols(gr)$protospacer <- DNAStringSet(mcols(gr)$protospacer)
    mcols(gr)$pam <- DNAStringSet(mcols(gr)$pam)
    values <- factor(mcols(gr)[[id.field]], levels=ids)
    names(gr) <- values
    grs <- S4Vectors::split(gr, f=values)[ids]
    return(grs)
}





#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom S4Vectors mcols<- DataFrame
#' @importFrom Biostrings DNAStringSet
.formatGeneAnnotation <- function(geneAnnotation,
                                  ids=NULL,
                                  id.field="ID"
){
    if ("chr" %in% colnames(geneAnnotation)){
        seqcol <- "chr"
    } else if ("seqnames" %in% colnames(geneAnnotation)){
        seqcol <- "seqnames"
    } else {
        stop("Cannot find chr or seqnames as column in geneAnnotation")
    }
    geneAnnotation <- DataFrame(geneAnnotation)
    geneAnnotation[["chr"]] <- geneAnnotation[[seqcol]]
    if (seqcol=="seqnames"){
        geneAnnotation[[seqcol]] <- NULL
    }
    rownames(geneAnnotation) <- geneAnnotation[[id.field]]
    values <- factor(geneAnnotation[[id.field]], levels=ids)
    dfs <- S4Vectors::split(geneAnnotation, f=values)[ids]
    return(dfs)
}


#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom S4Vectors mcols<- DataFrame
#' @importFrom Biostrings DNAStringSet
.formatTssAnnotation <- function(tssAnnotation,
                                 ids=NULL,
                                 id.field="ID"
){
    if ("chr" %in% colnames(tssAnnotation)){
        seqcol <- "chr"
    } else if ("seqnames" %in% colnames(tssAnnotation)){
        seqcol <- "seqnames"
    } else {
        stop("Cannot find chr or seqnames as column in tssAnnotation")
    }
    tssAnnotation <- DataFrame(tssAnnotation)
    tssAnnotation[["chr"]] <- tssAnnotation[[seqcol]]
    if (seqcol=="seqnames"){
        tssAnnotation[[seqcol]] <- NULL
    }
    rownames(tssAnnotation) <- tssAnnotation[[id.field]]
    values <- factor(tssAnnotation[[id.field]], levels=ids)
    dfs <- S4Vectors::split(tssAnnotation, f=values)[ids]
    return(dfs)
}



#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom S4Vectors mcols<- DataFrame
#' @importFrom Biostrings DNAStringSet
.formatEnzymeAnnotation <- function(enzymeAnnotation,
                                    ids=NULL,
                                    id.field="ID"
){
    enzymeAnnotation <- DataFrame(enzymeAnnotation)
    rownames(enzymeAnnotation) <- enzymeAnnotation[[id.field]]
    wh <- match(ids, rownames(enzymeAnnotation))
    enzymeAnnotation <- enzymeAnnotation[wh,,drop=FALSE]
    return(enzymeAnnotation)
}



#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom S4Vectors mcols<- DataFrame
#' @importFrom Biostrings DNAStringSet
.formatSnps <- function(snps,
                        ids=NULL,
                        id.field="ID"
){
    snps <- DataFrame(snps)
    rownames(snps) <- snps[[id.field]]
    wh <- match(ids, rownames(snps))
    snps <- snps[wh,,drop=FALSE]
    return(snps)
}










