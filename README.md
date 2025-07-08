# Fragmentomics R Package

## Description

This package analyze data from [nf-fragmentomics](https://github.com/sottorivalab/nf-fragmentomics) nextflow pipeline.

## Install

When you install from RStudio with the Install button, vignette are not installed or linked.

To build with the knitr vignette:

```         
devtools::install(build_vignettes = T)
```

And you can verify presence of vignette with:

```         
vignette("fragmentomics")
```

## Containers

This package is used in the following containers of [nf-fragmentomics pipeline](https://github.com/sottorivalab/nf-fragmentomics){.uri}:

1.  [fragmentomics singularity](https://cloud.sylabs.io/library/tucano/fragmentomics/fragmentomics_peak_stats)
2.  [fragmentomics docker](https://hub.docker.com/r/tucano/fragmentomics_peak_stats){.uri}

## Usage

See vignette in \`doc\` for some examples

## Development

Using roxygen2 for documentation:

To rebuild docs:

```         
devtools::document()
```

Verify devtools with:

```         
devtools::has_devel()
```

## 
