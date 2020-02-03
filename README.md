# Basilico_NCpaper_Code

## Xiaonan Wang
## 03Feb2020
## Summary: Re-generate plots used in Basilico's Nature Communication paper, in preparation

## Introduction
Leukaemogenic mutations commonly disrupt cellular differentiation and/or enhance proliferation, thus perturbing the regulatory programs that control self-renewal and differentiation of stem and progenitor cells. Translocations involving the Mll1 (Kmt2a) gene generate powerful oncogenic fusion proteins, predominantly affecting infant and paediatric AML and ALL patients. The early stages of leukaemogenic transformation are typically inaccessible from human patients and conventional mouse models. Here, we took advantage of cells conditionally blocked at the multipotent haematopoietic progenitor stage to develop a new MLL-r model capturing the early cellular and molecular consequences of MLL-ENL expression based on a clear clonal relationship between the parental and leukaemic cells. Through a combination of scRNA-seq, ATAC-seq and genome-scale CRISPR-Cas9 screening, we identified pathways and genes likely to drive the early phases of leukaemogenesis. Finally, we demonstrated the broad utility of using matched parental and transformed cells for small molecule inhibitor studies by validating both known and new potential therapeutic targets.

## Notebooks
- [Main mRNA Smart-Seq2 figures](https://github.com/SharonWang/Basilico_NCpaper_Code/blob/master/Code/SmartSeq2_Data_Analysis.ipynb): 
    * Regenerate main mRNA Smart-Seq2 data analysis plots (R code)
    * The data required for the analysis can be downloaded from xxx
    * The analysis was done using bglab package (Gottgens in-house pipeline), which can be downloaded from https://github.com/wjawaid/bglab
    
- [Projection onto Wilson landscape](https://github.com/SharonWang/Basilico_NCpaper_Code/blob/master/Code/Projection_onto_Wilson_Data.ipynb): 
    * Project mRNA Smart-Seq2 data onto Wilson haematopoeitic landscape ([PMID: 29588278](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5969381/)) (Python code based on [Scanpy](https://icb-scanpy.readthedocs-hosted.com/en/stable/))
