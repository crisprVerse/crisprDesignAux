#' @title Save GuideSet object to a BED file
#' 
#' @description Save GuideSet object to a BED file
#' 
#' @param guideSet A \code{GuideSet} object from crisprDesign.
#' @param filename String specifying filename of the BED file.
#' @param overwrite Should the file be overwritten if it exists?
#'     FALSE by default. 
#' @param trackName String specifying the name of the genomic track.
#' @param trackDescription String specifying description of the genomic track.
#' @param browserChr String specifying default chromosome location of the
#'     genomic track. "Chr3" by default.
#' @param browserStart Integer specifying default chromosome start of the 
#'     genomic track.
#' @param browserEnd Integer specifying default chromosome end of the 
#'     genomic track.
#' @param browserVisibility Integer specifying the default genomic track
#'     visibility.
#' @param scoreColumn String specifying column name in \code{mcols(guideSet)}
#'     to be used as the score for the BED file. NULL by default.
#' @param style Which bed format style should be used?
#'     For UCSC genome browser, specify "ucsc" (default).
#'     For Epiviz, specify "epiviz".
#' 
#' @return No object returned. Object is saved into a BED file as a side 
#'     effect. 
#' 
#' @author Jean-Philippe Fortin
#' 
#' @examples 
#' library(crisprDesign)
#' data(guideSetExampleFullAnnotation, package="crisprDesign")
#' GuideSetToBed(guideSetExampleFullAnnotation,
#'               filename=tempfile())
#' 
#' @export
#' @importFrom crisprDesign crisprNuclease
#' @importFrom stats complete.cases
#' @importFrom utils write.table
GuideSetToBed <- function(guideSet,
                          filename="output.bed",
                          overwrite=FALSE,
                          trackName="My gRNA track",
                          trackDescription="Something about my track",
                          browserChr="chr3",
                          browserStart=10141008,
                          browserEnd=10152220,
                          browserVisibility=3,
                          scoreColumn=NULL,
                          style=c("ucsc", "epiviz")
){
    style <- match.arg(style)
    itemRgb <- TRUE
    nuc <- crisprNuclease(guideSet)
    chr <- as.character(GenomeInfoDb::seqnames(guideSet))
    strand <- as.character(BiocGenerics::strand(guideSet))
    targetRanges <- crisprBase::getTargetRanges(gr=guideSet,
                                                nuclease=nuc)
    protospacerRanges <- crisprBase::getProtospacerRanges(gr=guideSet,
                                                          nuclease=nuc)
    pamRanges <- crisprBase::getPamRanges(gr=guideSet,
                                          nuclease=nuc)
    cutSiteRanges <- crisprBase::getCutSiteRanges(gr=guideSet,
                                                  nuclease=nuc)
    start <- BiocGenerics::start(targetRanges)-1
    end   <- BiocGenerics::end(targetRanges)
    thickStart <- BiocGenerics::start(protospacerRanges)-1
    thickEnd   <- BiocGenerics::end(protospacerRanges)
    pamStart   <- BiocGenerics::start(pamRanges)-1
    pamEnd     <- BiocGenerics::end(pamRanges)
    cutSite <- BiocGenerics::start(cutSiteRanges)
    ids <- names(guideSet)
    if (!is.null(scoreColumn)){
        score <- S4Vectors::mcols(guideSet)[[scoreColumn]]
    } else {
        score <- rep(945, length(chr))
    }

    # Adding sequences:
    spacers <- crisprDesign::spacers(guideSet,
                                     as.character=TRUE)

    outCore <-  data.frame(chr, start,end, ids, score, strand)
    if (style=="ucsc"){
        outAdd <- data.frame(thickStart, thickEnd)
    } else if (style=="epiviz"){
        outAdd <- data.frame(pamStart, pamEnd, cutSite, spacers)
    }
    out <- cbind(outCore, outAdd)


    # Some cleaning:
    out <- out[out$chr %in% paste0("chr", c(1:22, "X","Y")),,drop=FALSE]
    out <- out[complete.cases(out),,drop=FALSE]
    
    # Writing header
    line1 <- paste0("browser position ",
                    browserChr, ":",
                    browserStart, "-",
                    browserEnd)
    line2 <- paste0('track name="',trackName,
                    '" description="',trackDescription,
                    '" visibility=', browserVisibility)
    if (itemRgb){
        line2 <- paste0(line2, ' itemRgb="On"')
    }
    if (file.exists(filename) & !overwrite){
        stop("The specified file already exists and overwrite=FALSE.")
    }
    if (style=="ucsc"){
        writeLines(con=filename,
                   text=c(line1, line2))
    } 


    # Writing features:
    write.table(out,
                append=TRUE,
                file=filename,
                quote=FALSE,
                row.names=FALSE,
                col.names=FALSE)
}



#' @export
bedToBigBed <- function(bedFile,
                        species=c("human", "mouse"),
                        outputFile=gsub(".bed", ".bb", bedFile)
){
    species <- match.arg(species)
    if (species=="human"){
        chrFile <- system.file("chromSizes/hg38.chrom.sizes",
                               package="crisprDesignAux",
                               mustWork=TRUE)
    } else {
        chrFile <- system.file("chromSizes/mm10.chrom.sizes",
                               package="crisprDesignAux",
                               mustWork=TRUE)
    }

    .platform <- function(){
        info <- Sys.info()
        os   <- info[["sysname"]]
        if (os=="Darwin"){
            out <- "mac"
        }  else {
            out <- "linux"
        }
        return(out)
    }
    programDir <- paste0("binaries/ucsc/", .platform(),"_x86_64")
    programDir <- system.file(programDir,
                              package="crisprBinaries",
                              mustWork=TRUE)
    program <- file.path(programDir,"bedToBigBed")
    schemaFile <- system.file("epivizBedSchema/grna.as",
                              package="crisprDesignAux",
                              mustWork=TRUE)
    type <- "bed3+7"

    # Sorting
    sortedBedFile <- gsub(".bed", ".sorted.bed", bedFile)
    cmd <- paste0("sort -k1,1 -k2,2n ",
                  bedFile, " > ",
                  sortedBedFile)
    system(cmd)

    cmd <- paste0(program,
                  " -as=", schemaFile,
                  " -type=", type, 
                  " ", sortedBedFile,
                  " ", chrFile,
                  " ", outputFile)
    print(cmd)
    system(cmd)
}







