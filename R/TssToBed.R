TssToBed <- function(tssObject,
                     filename="output.bed",
                     overwrite=TRUE,
                     trackName="My TSS track",
                     trackDescription="Something about my track",
                     browserChr="chr3",
                     browserStart=10141008,
                     browserEnd=10152220,
                     browserVisibility=3,
                     itemRgb=TRUE
){
    chr <- as.character(GenomeInfoDb::seqnames(tssObject))
    strand <- as.character(BiocGenerics::strand(tssObject))     
    start <- BiocGenerics::start(tssObject) - 1
    end <- BiocGenerics::end(tssObject)
    ids <- names(tssObject)
    score <- rep(945, length(chr))
    out <- data.frame(chr,start, end,
                      ids, score, strand)
    out <- out[out$chr %in% paste0("chr", c(1:22, "X", "Y")), 
        , drop = FALSE]
    out <- out[complete.cases(out), , drop = FALSE]
    line1 <- paste0("browser position ", browserChr, ":", browserStart, 
        "-", browserEnd)
    line2 <- paste0("track name=\"", trackName, "\" description=\"", 
        trackDescription, "\" visibility=", browserVisibility)
    if (itemRgb) {
        line2 <- paste0(line2, " itemRgb=\"On\"")
    }
    if (file.exists(filename) & !overwrite) {
        stop("The specified file already exists and overwrite=FALSE.")
    }
    writeLines(con=filename,
               text=c(line1, line2))
    write.table(out,
                append=TRUE,
                file=filename,
                quote=FALSE,
                row.names=FALSE,
                col.names=FALSE)
}