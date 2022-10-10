
rmarkdown::render(input = "README.Rmd",
                  output_format = "github_document",
                  output_options = list(
                    name_value_pairs = "value",
                    or_something = TRUE,
                    toc = TRUE,
                    df_print = "tibble"
                  )
)
