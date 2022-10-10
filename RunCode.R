
rmarkdown::render(input = "Monti_Patnaik_project2T_ST558.Rmd",
                  output_file = "README.md",
                  output_format = "github_document",
                  output_options = list(
                    name_value_pairs = "value",
                    or_something = TRUE,
                    toc = TRUE,
                    df_print = "tibble"
                  ),
                  runtime = "static",
                  clean = TRUE,
                  params = NULL,
                  knit_meta = NULL,
                  envir = parent.frame(),
                  run_pandoc = TRUE,
                  quiet = FALSE,
                  encoding = "UTF-8"
)

