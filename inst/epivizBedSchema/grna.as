table grna
"CRISPR gRNA specification"
    (
    string chrom;      "Chromosome"
    uint chromStart;   "Start position of the target sequence, including the PAM sequence"
    uint chromEnd;     "End position of the target sequence, including the PAM sequence"
    string name;       "Name of item, for display"
    uint score;        "Score from 0-1000"
    string strand;     "Strand of the target sequence"
    uint pamStart;     "Start position of the PAM sequence"
    uint pamEnd;       "End position of the PAM sequence"
    uint cutSite;      "Position of the cut site"
    string spacerSequence;     "Spacer DNA sequence"
    )