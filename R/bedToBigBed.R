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

