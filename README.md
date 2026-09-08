# Clock In & Out analysis

An end-to-end personal analytics case study built from 250 days of office
clock-in and clock-out records. The Quarto report covers collection, cleaning,
tidying multi-label events, descriptive summaries, and interactive Plotly
visualisation in R.

**[Read the published analysis](https://tinomuzambi.github.io/ClockInOut/)**

## What this demonstrates

- reshaping multi-label event data into tidy indicator columns
- handling dates, times, missing values, and non-office days with `lubridate`
- comparing arrival, departure, and office-duration patterns by event type
- communicating results with static `ggplot2`, interactive Plotly, and tables

## Reproduce it

Install R, Quarto, `tidyverse`, `lubridate`, `plotly`, `DT`, and `scales`. Place
a private export at `data/data.csv` with the columns `Date`, `Clock In Time`,
`Clock Out Time`, and `Notes`, then run:

```bash
quarto render "Clock In Out Analysis.qmd"
```

Raw work records are intentionally ignored and are not distributed. The
committed `index.html` is a self-contained snapshot with only the report output
chosen for publication.

Code and original prose are available under the MIT license.
