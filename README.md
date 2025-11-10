# Elections Visualised: Mapping UK General Elections (2010–2024)

This repository contains the source files for **Elections Visualised**, a teaching and research project examining the geography of voting in UK General Elections from 2010 to 2024.
It demonstrates how to **acquire, clean, analyse, and visualise geospatial and electoral data in R**, with a focus on transparent and reproducible workflows.

---

## Repository Structure

```
├── _quarto.yml        # Quarto project configuration
├── index.qmd          # Intro and overview
├── results/           # Maps, figures, and narrative analysis
├── methods/           # Data processing & mapping tutorials
├── data/              # Election results & shapefiles (or links)
└── references/        # Sources and bibliography
```

---

## Data Sources

* **House of Commons Library** — UK General Election Results (1918–2024)
* **Office for National Statistics (ONS)** — Parliamentary constituency boundaries (2021 / 2024)

---

## Getting Started

To run locally:

```r
# install dependencies
install.packages(c("data.table", "fixest", "ggiraph", "gt", "here", "leaflet","lubridate", 
          "janitor", "purrr","readxl", "scales", "sf" ,"tidyverse"))
```

---

## Status

**Work in Progress**
Additional chapters — including **Spatial Regression** and advanced geospatial exercises — will be added soon.


---

## Contact

For academic collaboration, teaching use, or questions, feel free to reach out.