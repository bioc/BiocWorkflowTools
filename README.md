# BiocWorkflowTools

### Tools to aid the development of Bioconductor Workflow packages.

**BiocWorkflowTools** provide a mechanism to assist in converting between R Markdown and LaTeX documents, specifically in the case where one is writing an anaylsis workflow to be submitted to F1000Research, with the intention to also host a runable example on Bioconductor.  Reaching these two endpoints while maintaining a single working document can be challenging.  Submission to the journal requires a LaTeX file, which is best achieved by writing an Rnw workflow based on **Sweave** or **knitr**, while producing the html based versions hosted by Bioconductor is most easily achieved from an R Markdown document.  **BiocWorkflowTools** allows an author to convert a R Markdown document to a submission ready LaTeX document with all the appropriate formating applied automatically.

## Current Status

| Travis        | BioC        | 
| ------------- |-------------| 
| [![Build Status](https://travis-ci.org/grimbough/BiocWorkflowTools.svg?branch=master)](https://travis-ci.org/grimbough/BiocWorkflowTools) | [![BioC Status](https://bioconductor.org/shields/build/devel/bioc/BiocWorkflowTools.svg)](http://bioconductor.org/checkResults/devel/bioc-LATEST/BiocWorkflowTools/) 

## Contact

For bug reports, please register an [issue](https://github.com/grimbough/BiocWorkflowTools/issues) here on Github. For usage queries please post a question on the [Bioconductor Support Forum](https://support.bioconductor.org/p/new/post/?tag_val=BiocWorkflowTools).


## Funding 

Funding for continued development and maintenance of this package is provided by the German Network for Bioinformatics Infrastructure

<a href="http://www.denbi.de"><img src="https://tess.elixir-europe.org/system/content_providers/images/000/000/063/original/deNBI_Logo_rgb.jpg" width="400" align="left"></a>
