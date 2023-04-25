library(crisprDesignAux)
library(crisprDesign)
library(crisprDesignGne)
library(crisprDesignData)
species <- "human"
bsgenome=getGenomePackage(species)
if (species=="human"){
    txdb <- txdb_human
} else {
    txdb <- txdb_mouse
}
gene <- ifelse(species=="human", "KRAS", "Kras")

gr <- crisprDesign::queryTxObject(txdb,
                                  featureType="cds",
                                  queryValue=gene,
                                  queryColumn="gene_symbol")

nuc <- SpCas9
gs <- findSpacers(gr,
                  crisprNuclease=nuc,
                  bsgenome=bsgenome)
gs <- unique(gs)
bedFile <- "kras_cas9_human.bed"
GuideSetToBed(gs,
              overwrite=TRUE,
              filename=bedFile,
              style="epiviz")
bedToBigBed(bedFile)


nuc <- enAsCas12a
gs <- findSpacers(gr,
                  crisprNuclease=nuc,
                  bsgenome=bsgenome)
gs <- unique(gs)
bedFile <- "kras_cas12a_human.bed"
GuideSetToBed(gs,
              overwrite=TRUE,
              filename=bedFile,
              style="epiviz")
bedToBigBed(bedFile)



