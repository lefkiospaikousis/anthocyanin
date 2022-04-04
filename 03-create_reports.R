

render_variety <- function(variety){
  
  rmarkdown::render(
    input = "per_variety.rmd",
    output_file = paste0("variety-", variety, ".docx"),
    params = list(variety = variety),
    envir = new.env()
  )
}


render_variety("Maratheftiko")

render_variety("OFTHALMO")

render_variety("Cabernet Sauvignon")

render_variety("Mavro")


# render everything

dta_full <- readRDS("Data/anthocyans-clean.rds")

  purrr::walk(
    unique(dta_full$variety),
    ~ render_variety(.)
  )


