
#theme gtsummary

theme_gtsummary <-   
  list(
    # report median (IQR) and n (percent) as default stats in `tbl_summary()`
    "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 2),
    "tbl_summary-str:continuous_stat" = "{mean} ({sd})",
    "tbl_summary-arg:digits" = list(all_continuous() ~ c(1, 1)),
    "add_p.tbl_summary-attr:test.continuous_by2" = "t.test",
    "add_p.tbl_summary-attr:test.continuous" = "aov",
    "pkgwide-str:theme_name" = "Improvast Theme"
  )



#' Create wide table of frequencies (%) . SPSS like 
#' 
#' Supply a dataset with likert scale items and get a `wide`table with the items as rows
#' and the likert respones as columns.
#' 
#' @details Make sure the dataset supplied is only the columns you want.Similar usage to \code{tbl_summary} functon
#' of the \code{gtssummary} package
#' @param data The dataset
#' @param ... Other arguments passed to \code{tbl_summary} function
aggregate_wide <- function(data, ...){
  
  
  gt_table <- 
    data %>% 
    tbl_summary(...) 
  
  var_labels <- gt_table$meta_data$var_label
  
  miss <- gt_table$inputs$missing
  miss_text <- gt_table$inputs$missing_text
  
  wide_table <- 
    gt_table %>% 
    as_tibble() %>% 
    set_names("name","values") %>% 
    mutate(
      item  = if_else(name %in% var_labels,  name, NA_character_)
    ) %>% 
    fill(item) %>% 
    na.omit() %>% 
    mutate(name = fct_inorder(name),
           item = fct_inorder(item)) %>% 
    spread(name, values) %>% 
    mutate(item  = stringr::str_squish(item)) %>% 
    relocate(item)
  
  wide_table
  
  #TODO relocate does not work if "Unknown" (missing_text) is presenet
  # I think now its done .. :)
}


#' Sets the Greek locale 
set_greek_locale <- function(){
  Sys.setlocale(locale = "greek")
  
}


#function to compliment aggrigate_wide

compliment_aw<-function(data,match){
  data %>% 
    select(matches(match)) %>% 
    aggregate_wide(
      missing = "no"
    )}


score <- function(...,  Means = TRUE, na.rm =  TRUE){
  if(Means){
    rowMeans(dplyr::across(...), na.rm = na.rm)
  } else {
    rowSums(dplyr::across(...), na.rm = na.rm)
  }
}


#function for correlation

cor_function <- function(dim1,dim2) {
  
  list(var1 = dim1, var2 = dim2) %>% 
    cross_df() %>% 
    mutate(
      cor_test = map2(var1, var2, ~ cor.test(dta[[.x]], dta[[.y]])),
      r = map_dbl(cor_test, ~ .$estimate),
      p = map_dbl(cor_test, ~ .$p.value),
      var1 = recode(var1, !!!unlist(labels)),
      var2 = recode(var2, !!!unlist(labels))
    ) %>% 
    mutate(r = round(r,2)) %>% 
    mutate(sig = case_when(
      p <0.01 ~ "**",
      p <0.05~ "*",
      TRUE ~ ""
    ),
    r = paste0(r, sig)  ) %>% 
    select(var1, var2, r) %>% 
    pivot_wider(
      names_from = var2,
      values_from = r
    ) 
}


#' Descriptives of variables by a factor
#' #
#' @param dta The dataset
#' @param group_var The variable to group_by. 
#' @param ... Thge variables the get the descriptives. You can use dplyr style for this

descr_by_group <- function(dta, group_var, ...) {
  
  dta  %>% 
    # TODO Use the `all_of` when using objects that contain columns in a character format
    select(
      {{group_var}}, ...
    ) %>%
    tbl_summary(
      by = {{group_var}},
      missing = "no"
      , type = list(all_continuous() ~ "continuous")
      #digits = all_continuous() ~ c(1, 1)
    ) %>% 
    add_overall() %>% 
    add_p(test = list(all_categorical() ~ "chisq.test"
                      #all_continuous() ~ "t.test"
    )
    ) %>% 
    #as_tibble() %>% 
    {.}
  
}


#descriptives function---------------------------------------------------------

#' @param dta The dataset
#' 
descriptives_fun <- function(dta, dim){
  dta %>%
    select(all_of(dim)) %>% 
    pivot_longer(
      cols = everything(),
      names_to = "Scale",
      values_to = "value"
      
    ) %>%
    mutate(value = round(value, 1)) %>% 
    group_by(Scale) %>%
    summarise(`Mean (SD)` = paste(round(mean(value),digits = 1)," (",
                                  round(sd(value),digits = 1),")",sep = ""),
              ObservedRange=paste("[",range(value)[1],"-",range(value)[2],"]",sep = "")) %>%
    arrange(factor(Scale, levels = dim))
}


plot_sina <- function(data, ..., .by, .split_by = NULL){
  
  dta_long <- 
    data %>% 
    select(..., {{.by}}, {{.split_by}}) %>% 
    pivot_longer(
      names_to = "key",
      values_to = "value",
      cols = -c({{.by}}, {{.split_by}})
    ) %>% 
    mutate(key = fct_inorder(key)) 
  
  dta_long %>% 
    ggplot(aes(x = {{.by}}, y = value, fill = {{.split_by}}))+
    #geom_boxplot()+
    geom_violin()+
    ggforce::geom_sina(alpha=0.5)+
    coord_flip()+
    scale_x_discrete(labels = scales::wrap_format(10))+ #function(x) str_wrap(x, width = 10))+
    facet_wrap(~key, scales = "free_x", labeller = as_labeller(unlist(labels)))+
    theme_bw(base_size = 14)+
    theme(legend.position = "top") 
}

