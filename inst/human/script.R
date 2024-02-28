a <- readRDS("../../../crisprAnnotation-data/v5/annotations_comprehensive/crisprko.cas9.human.cropseq.pooled.rds")
a$source <- gsub("gne", "", a$source)
gsHuman <- a
save(gsHuman, file="gsHuman.rda")
