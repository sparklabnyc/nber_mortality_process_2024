# nber_mortality_process_2024

Project description

Created by Yuanyu Lu (<yl5509@cumc.columbia.edu>)

Started on 2024-05-31

## Project description

NBER Mortality Process 2024

Aims:

- Aim 1. File `R/model.R`.

Findings:

- Findings 1. File `analysis/`
- Findings 2. File `reports/`

## Directory structure

```md
.
├── .gitignore
├── .pre-commit-config.yaml
├── .github/workflows/pre-commit.yml
├── README.md
├── renv.lock
├── renv
├── nber_mortality_process_2024.Rproj
├── data
│   └── subfolder
├── R
│   ├── model.R
│   ├── preprocessing.R
│   └── utils.R
├── output
├── analysis
│   └── analysis.qmd
└── reports
```

| Folder     | File              | Description                                            |
|------------|-------------------|--------------------------------------------------------|
| `data`     | `population.csv`  | Population data                                        |
|            | `shapefile.json`  | Shapefile                                              |
| `R`        | `preprocessing.R` | Data preprocessing steps                               |
|            | `model.R`         | Model                                                  |
|            | `utils.R`         | Common reusable R functions                            |
| `output`   | `processed.csv`   | Modelled data                                          |
| `analysis` | `analysis.qmd`    | Main data analysis for finding X                       |
| `reports`  | `paper.docx`      | Final paper                                            |

## Running the code

We use [`renv`](https://rstudio.github.io/renv/articles/renv.html) for package dependency management and reproducibility.
You will need to run `install.packages('renv')` in your base `R` distribution.

To get the same packages as we used, run `renv::restore()` and then run the scripts.

## Data availability

Data used in the analysis are controlled by the XX who do not have permission to release data to third parties.
Individual mortality data can be requested through XX (e.g. the US CDC).
If you would like a file containing simulated data that allow you to test the code, please contact <yl5509@cumc.columbia.edu>.
