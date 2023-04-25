#' @title Retrieve a GuideSet object from a SQLite database
#' 
#' @description Retrieve a GuideSet object from a SQLite database.
#' 
#' @param df Data.frame containing gRNA information
#' @param crisprNuclease \code{CrisprNuclease} object from the 
#'     crisprBase package representing the nuclease used to 
#'     build the database.
#' @param bsgenome \code{BSgenome} object representing the genome
#'     used to build the database. 
#' @param idCol String specifying the name of the column
#'    used to store gRNA ids.
#' @param spacerCol String specifying the column containing
#'     the spacer sequences. "spacer-20mer" by default. 
#' @param dropNtcs Should non-mapping gRNAs be dropped?
#'     TRUE by default. 
#' @param keepColumns Should additional annotation columns be added
#'     to the GuideSet object? TRUE by default.
#' @param verbose Should messages be printed to console? 
#'     TRUE by default. 
#' 
#' @details The following columns must exist in the data.frame to be 
#'    able to construct a \code{GuideSet} object:
#' 
#' \itemize{
#' \item \code{chr} String specifying chromosome information.
#'      Column \code{seqnames} can be provided instead
#' \item \code{pam_site} PAM site of the found protospacer.
#' \item \code{strand} Strand of the protospacer sequence. 
#' \item \code{chr} String specifying chromosome information.
#'     Column \code{seqnames} can be provided instead#' 
#' }
#' 
#' Moreover, the column specified by \code{idCol} must exist and 
#' represent unique gRNA ids. The column specified by \code{spacerCol} must 
#' also exist and represent the spacer sequences. 
#' 
#' @author Jean-Philippe Fortin
#' 
#' @examples 
#' df <- data.frame(chr=c("chr1", "chr2"),
#'                  pam_site=c(10000920, 10000923),
#'                  strand=c("+", "-"),
#'                  pam=c("AGG","TGG"),
#'                  spacer=c("AGTGTCGTGTGTGTGTGTGT", "CCCCTCGTGTGTGTGTTTTT"),
#'                  id=c("grna1", "grna2"),
#'                  score1=c(0.5, 0.4),
#'                  score2=c(0.1, 0.2))
#' library(crisprBase)
#' library(BSgenome.Hsapiens.UCSC.hg38)
#' bsgenome <- BSgenome.Hsapiens.UCSC.hg38
#' data(SpCas9, package="crisprBase")
#' guideSet <- DataFrameToGuideSet(df,
#'                                 crisprNuclease=SpCas9,
#'                                 bsgenome=bsgenome,
#'                                 idCol="id", 
#'                                 spacerCol="spacer")
#' 
#' @export 
#' @importFrom crisprDesign GuideSet
#' @importFrom S4Vectors mcols<- mcols
#' @importFrom GenomeInfoDb seqinfo seqinfo<-
#' @importFrom GenomeInfoDb seqlevels seqlevels<-
DataFrameToGuideSet <- function(df,
                                crisprNuclease,
                                bsgenome,
                                idCol="ID",
                                spacerCol = "spacer_20mer",
                                dropNtcs=TRUE,
                                keepColumns=TRUE,
                                verbose=TRUE
){
    if (!"pam_site" %in% colnames(df)){
        stop("pam_site must be a column of the data.frame")
    }
    if (!"pam" %in% colnames(df)){
        stop("pam must be a column of the data.frame")
    }
    if (!"strand" %in% colnames(df)){
        stop("strand must be a column of the data.frame")
    }
    if ("seqnames" %in% colnames(df)){
        chr <- df$seqnames
    } else if ("chr" %in% colnames(df)){
        chr <- df$chr
    } else {
        stop("seqnames or chr must be provided as a column in the data.frame.")
    }
    if (dropNtcs){
        bad <- which(is.na(chr))
        nbad <- length(bad)
        if (nbad>0){
            df <- df[-bad,,drop=FALSE]
            chr <- chr[-bad]
            if (verbose){
                msg <- paste0("[DataFrameToGuideSet] Dropping ", nbad, " NTCs. \n")
                cat(msg)
            }
        }
    }
    if (!idCol %in% colnames(df)){
        stop("The column specified by idCol cannot be found.")
    }
    if (!spacerCol %in% colnames(df)){
        stop("The column specified by spacerCol cannot be found.")
    }
    df <- df[!is.na(df$pam_site),,drop=FALSE]
    gs <- GuideSet(ids=df[[idCol]],
                   pam_site=df$pam_site,
                   protospacers=df[[spacerCol]],
                   pams=df$pam,
                   strand=df$strand,
                   seqnames=chr,
                   CrisprNuclease=crisprNuclease,
                   bsgenome=bsgenome)

    if (keepColumns){
        gs_cols <- colnames(mcols(gs))
        df_cols <- colnames(df)
        cols <- setdiff(df_cols, gs_cols)
        cols <- setdiff(cols,
                        c("chr", "seqnames", "strand", "start", "end"))
        cols <- setdiff(cols, c(spacerCol, idCol))
        if (length(cols)>0){
            mcols(gs)[cols] <- df[, cols,drop=FALSE]
        }
    }
    seq <- seqinfo(gs)
    seqlevels(seq) <- seqlevels(seqinfo(bsgenome))
    seqlevels(gs) <- seqlevels(seq)
    seqinfo(gs) <- seq   
    return(gs)
}

