# Pakistan Independent Power Producers

Research by Andrew Wang and Hassan Aziz on whether Chinese sponsorship protects
Pakistan's Independent Power Producers (IPPs) from contract renegotiation. The
project combines a manuscript on Vertical Political Integration, renegotiation
data, exploratory survival analysis, and Business Recorder news scraping.

## Project layout

| Directory | Contents |
| --- | --- |
| `data/` | `ipps.csv`: renegotiation events, sponsorship, capacity, and source links; `ipps_analysis.R`: Cox models, simulations, plots, and a logit comparison. |
| `scraping/` | News scraper, article-processing helpers, saved article index, and IPP/group dictionaries with aliases. |
| `manuscript/` | `main.tex`, `references.bib`, the manuscript figure, and the existing `IPPs.pdf`. |

## Run the R code

Open `ipps.Rproj` in RStudio, or set the working directory to this repository's
root. All R file paths are relative to that directory.

Required packages:

- Analysis: `tidyverse`, `simcf`, `tile`, `MASS`, `survival`, `RColorBrewer`, `stargazer`.
- Scraping and article processing: `rvest`, `xml2`, `dplyr`, `lubridate`, `httr2`, `stringi`, `stringr`, `purrr`, `tibble`.

```r
source("data/ipps_analysis.R")       # Models and plots; 10,000 simulation draws
source("scraping/ippstimeline.R")    # Loads helpers and runs a synthetic example
```

To refresh the article index, adjust `keyword` and `pages` in
`scraping/BR_webscraper.R`, then source it. This accesses the network and replaces
`scraping/br_keyword_ipps.csv` for the default keyword. The commented example in
`ippstimeline.R` shows how to fetch article bodies and scan them for IPPs/groups;
no browser executable is required. Live scraping depends on site availability
and HTML structure.

## Build the manuscript

With a LaTeX distribution providing `latexmk`, pdfLaTeX, and BibTeX, run from the
repository root:

```sh
latexmk -cd -pdf manuscript/main.tex
```

This resolves the figure and bibliography inside `manuscript/` and produces
`manuscript/main.pdf`. The existing `IPPs.pdf` is a separate saved output.

The research and data collection are ongoing. Analysis results and the manuscript
figure/table are maintained separately; running R does not update the LaTeX
manuscript automatically. Some bibliography entries still omit source URLs.
