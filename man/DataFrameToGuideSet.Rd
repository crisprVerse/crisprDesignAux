% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataFrameToGuideSet.R
\name{DataFrameToGuideSet}
\alias{DataFrameToGuideSet}
\title{Retrieve a GuideSet object from a SQLite database}
\usage{
DataFrameToGuideSet(
  df,
  crisprNuclease,
  bsgenome,
  idCol = "ID",
  spacerCol = "spacer_20mer",
  dropNtcs = TRUE,
  keepColumns = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{df}{Data.frame containing gRNA information}

\item{crisprNuclease}{\code{CrisprNuclease} object from the 
crisprBase package representing the nuclease used to 
build the database.}

\item{bsgenome}{\code{BSgenome} object representing the genome
used to build the database.}

\item{idCol}{String specifying the name of the column
used to store gRNA ids.}

\item{spacerCol}{String specifying the column containing
the spacer sequences. "spacer-20mer" by default.}

\item{dropNtcs}{Should non-mapping gRNAs be dropped?
TRUE by default.}

\item{keepColumns}{Should additional annotation columns be added
to the GuideSet object? TRUE by default.}

\item{verbose}{Should messages be printed to console? 
TRUE by default.}
}
\description{
Retrieve a GuideSet object from a SQLite database.
}
\details{
The following columns must exist in the data.frame to be 
   able to construct a \code{GuideSet} object:

\itemize{
\item \code{chr} String specifying chromosome information.
     Column \code{seqnames} can be provided instead
\item \code{pam_site} PAM site of the found protospacer.
\item \code{strand} Strand of the protospacer sequence. 
\item \code{chr} String specifying chromosome information.
    Column \code{seqnames} can be provided instead#' 
}

Moreover, the column specified by \code{idCol} must exist and 
represent unique gRNA ids. The column specified by \code{spacerCol} must 
also exist and represent the spacer sequences.
}
\examples{
df <- data.frame(chr=c("chr1", "chr2"),
                 pam_site=c(10000920, 10000923),
                 strand=c("+", "-"),
                 pam=c("AGG","TGG"),
                 spacer=c("AGTGTCGTGTGTGTGTGTGT", "CCCCTCGTGTGTGTGTTTTT"),
                 id=c("grna1", "grna2"),
                 score1=c(0.5, 0.4),
                 score2=c(0.1, 0.2))
library(crisprBase)
library(BSgenome.Hsapiens.UCSC.hg38)
bsgenome <- BSgenome.Hsapiens.UCSC.hg38
data(SpCas9, package="crisprBase")
guideSet <- DataFrameToGuideSet(df,
                                crisprNuclease=SpCas9,
                                bsgenome=bsgenome,
                                idCol="id", 
                                spacerCol="spacer")

}
\author{
Jean-Philippe Fortin
}
