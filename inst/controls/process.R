essentialGenes <- rownames(read.table("AchillesCommonEssentialControls.csv"))
x <- readLines("AchillesNonessentialControls.csv")
x <- strsplit(x, split=" ")
x <- vapply(x, function(xx) xx[[1]], FUN.VALUE="a")
nonEssentialGenes <- xx
save(essentialGenes, file="../../data/essentialGenes.rda")
save(nonEssentialGenes, file="../../data/nonEssentialGenes.rda")
