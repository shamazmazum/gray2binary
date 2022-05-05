# gray2binary
[![CI](https://github.com/shamazmazum/gray2binary/actions/workflows/build.yml/badge.svg)](https://github.com/shamazmazum/gray2binary/actions/workflows/build.yml)

Tool for creating black&white segmented images from grayscale images of porous
material. This tool uses watershed segmentation algorithm.

~~~~
Usage: gray2binary [-l|--low LOW] [-h|--high HIGH] [-q|--quantile Q] INPUT-NAME OUTPUT-NAME

Available options:
  -l, --low LOW    Pixels below this threshold are labeled as void
  -h, --high HIGH  Pixels above this threshold are labeled as solids
  -q, --quantile Q Q-th and (1-Q)-th quantiles for void and solid thresholds
~~~~

Options `-q`, `-l` and `-h` can be used for tuning thresholds for black (void)
and white (solid) areas.

## Caveats

* Portable bitmap is the only output format supported at the moment.
* Some fine tuning of `-l` and `-h` parameters may be required to get a good result.
* Processing of big images can take some time.

## Examples

| Original           | Segmented               |
|--------------------|-------------------------|
| ![](imgs/gray.png) | ![](imgs/segmented.png) |

Original image was taken from:

Scott, G. (2020, January 16). North Sea Sandstone SEM Images. Retrieved May 05, 2022, from www.digitalrocksportal.org
