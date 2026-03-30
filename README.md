# vastR <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

An R implementation of the **Visual Argument Structure Tool (VAST)** by
Leising, Grenke, & Cramer (2023, *Meta-Psychology*).

VAST provides a visual language for mapping argument structures,
distinguishing between concepts, their names, different types of
relationships (causation, implication, prediction, transformation,
reasoning), beliefs (IS/OUGHT), and perspectives. This package generates
[Graphviz](https://graphviz.org/) DOT code and renders diagrams via
[DiagrammeR](https://rich-iannone.github.io/DiagrammeR/).

## Installation

Install from a local source (e.g., after cloning this repo):

```r
# Install dependencies
install.packages("DiagrammeR")

# Install vastR from local source
install.packages("path/to/vastR", repos = NULL, type = "source")

# Or using devtools:
# devtools::install("path/to/vastR")
```

Once published, install from GitHub:

```r
# install.packages("devtools")
devtools::install_github("finn-luebber/vastR")
```

## Quick Start

```r
library(vastR)

model <- vast_model(title = "Simple Example", naming_mode = "fimm") |>
  add_nodes(
    vast_concept("Smoking",    "Smoking"),
    vast_concept("LungCancer", "Lung Cancer"),
    vast_concept("TarBuildup", "Tar Buildup")
  ) |>
  add_edges(
    vast_causation("Smoking",    "TarBuildup", strength = 0.7),
    vast_causation("TarBuildup", "LungCancer", strength = 0.5)
  )

vast_render(model)
```

## Features

**All VAST element types:**

| Element | Function | Shape |
|---------|----------|-------|
| Concept | `vast_concept()` | Box |
| Name | `vast_name()` | Dashed box |
| Data | `vast_data()` | Box with thick left border |
| IS statement | `vast_is()` | Pentagon |
| OUGHT statement | `vast_ought()` | Pentagon |
| Perspective | `vast_perspective()` | Ellipse |
| Connective / Function | `vast_diamond()` | Diamond |

**All VAST relationship types:**

| Relationship | Function | Type |
|-------------|----------|------|
| Naming | `vast_naming()` | n |
| Conceptual implication | `vast_implication()` | i |
| Causation | `vast_causation()` | c |
| Transformation | `vast_transformation()` | t |
| Prediction | `vast_prediction()` | p |
| Reasoning | `vast_reasoning()` | r |
| Unknown | `vast_unknown()` | u |
| Noise | `vast_noise()` | — |

**Additional features:**

- Three naming modes: `"integrated"`, `"separated"`, `"fimm"` — switch display style with one parameter
- Higher-order concepts via `vast_group()` (bounding boxes around elements)
- Nested groups via `vast_nested_group()`
- Compound edges (`lhead`/`ltail`) — arrows to/from group borders
- Arbitrary content in diamonds (custom functions, formulas)
- Diamond footnotes for long formulas (`footnote = TRUE`)
- Numeric, verbal, or special strength markers on all relationships
- Edge validation — informative errors when edge IDs contain typos
- Export to SVG and PNG
- Pipe-friendly API (`|>`)

## Examples

See `inst/examples/` for worked examples reproducing figures from the
paper, or run:

```r
vignette("getting-started", package = "vastR")
```

## Acknowledgments

The code for this package was developed with substantial assistance
from Claude (Anthropic). Claude generated the initial package
architecture, the DOT rendering engine, and the majority of the
function implementations based on the author's design decisions,
specifications, and iterative feedback. The author is responsible for
the overall design, all conceptual decisions, testing, and maintenance.

This package implements the VAST framework by Leising, Grenke, &
Cramer (2023). The original authors are not affiliated with this
software implementation.

## Reference

Leising, D., Grenke, O., & Cramer, M. (2023). Visual Argument Structure
Tool (VAST) Version 1.0. *Meta-Psychology*, 7.
[doi:10.15626/MP.2021.2911](https://doi.org/10.15626/MP.2021.2911)

## License

MIT
