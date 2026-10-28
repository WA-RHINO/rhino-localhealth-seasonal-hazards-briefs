
#  DEFINE FUNCTIONS

# Time series plot code -- adopt and improve
# https://cdcgov.github.io/Rnssp/articles/Rnssp_trend_alert_detection.html


timeseries_plot_weather <- function(df, metric, alert, title, ylab, weatherfactor = c("Temp", "AQ"), weatherlab ){
  
  # Ensure 'weatherfactor' is one of the allowed values
  weatherfactor <- match.arg(weatherfactor)
  
  #if it exists plot weather data, will not exist if geography has no weather data in ESSENCE
  if(weatherfactor == "Temp" & exists("ts_weather")){
    scaletemp <- max(df[[metric]], na.rm = TRUE) / max(ts_weather$temp, na.rm = TRUE)
    
  } else if (weatherfactor == "AQ" & exists("ts_airquality")){
    scaleaq <- max(df[[metric]], na.rm = TRUE) / max(ts_airquality$aq, na.rm = TRUE)
  } 
  
  ggplot(data=df) +
    # Points for first axis
    geom_point(data =  df %>% filter(.data[[alert]] %in% c("red", "yellow")),  aes(x = date, y = .data[[metric]], shape = "Alerts", color = .data[[alert]]), size=2) +
    # Line for first axis
    geom_line( aes(x = date, y = .data[[metric]], linetype = "ED Visits"), color ="#254B73") +
    
    # Temp Line for second axis (scaled to match primary axis)
    {if(exists("ts_weather") & weatherfactor == "Temp")ggplot2::geom_line(data = ts_weather, aes(x=date, y=temp*scaletemp, linetype = "Temp (F)"), color = "darkgrey")} + 
    
    # AQ Line for second axis (scaled to match primary axis)
    {if(exists("ts_airquality") & weatherfactor == "AQ")ggplot2::geom_line(data = ts_airquality, aes(x=date, y=aq*scaleaq, linetype = "PM2.5 (ug/m^3)"), color = "darkgrey")} + 
    
    
    # Primary y-axis
    scale_y_continuous(
      limits = c(0, NA),
      name = ylab
    )  +
    
    # Secondary axis transformation
    {if(exists("ts_weather")  & weatherfactor == "Temp")ggplot2::scale_y_continuous(
      limits = c(0, NA),
      sec.axis = sec_axis(~ . / scaletemp, name = weatherlab)
    )} +
    
    # Secondary axis transformation
    {if(exists("ts_airquality")  & weatherfactor == "AQ")ggplot2::scale_y_continuous(
      limits = c(0, NA),
      sec.axis = sec_axis(~ . / scaleaq, name = weatherlab)
    )} +
    
    # Point colors
    scale_color_manual(
      #name = "Alerts",
      name = NULL,
      values = c( "red" = "#A50026", "yellow" = "#FDD081"),
      labels = c(  "red" = "Alert","yellow"= "Warning" )
    ) +
    # Shapes
    scale_shape_manual(
      name = NA,
      values = c("Alerts" = 16) # solid circle
    ) +
    scale_linetype_manual(
      name = NULL,
      values = c("ED Visits" = "solid")
    ) +
    
    {if(exists("ts_weather") & weatherfactor == "Temp")ggplot2::scale_linetype_manual(
      name = NULL,
      values = c("ED Visits" = "solid", "Temp (F)" = "dashed")
    )} +
    
    {if(exists("ts_airquality") & weatherfactor == "AQ")ggplot2::scale_linetype_manual(
      name = NULL,
      values = c("ED Visits" = "solid", "PM2.5 (ug/m^3)" = "dashed")
    )} +
    
    # Legend overrides to hide unwanted shapes/lines
    # Legend guides
    guides(
      shape = "none",
      color = guide_legend(order = 1),
      linetype = guide_legend(order = 2, override.aes = list(shape = NA))
    ) +
    scale_x_date(
      breaks = breaks_pretty(n = 6),
      #date_breaks = "2 days",
      name = "Date"
    ) +
    theme_light() +
    theme(
      legend.position = "bottom",
      axis.title.x = element_blank(),
      strip.background = element_rect(fill="#203864"),
      panel.background = element_rect(color = "#cfd3dd"),
      panel.grid = element_line(color = "#cfd3dd"),
      panel.border = element_rect(colour = "#cfd3dd"),
      axis.ticks = element_line(color = "#cfd3dd"),
      axis.text.x = element_text(angle = 0, vjust = .5, hjust = 0.5, size = 8), 
      plot.title = element_text(size = 11),
      axis.title.y = element_text(size = 8)
      #panel.grid.minor = element_blank
    ) +
    labs(title = title) 
}



# DATA DETAILS CLEANING ----

# Write a data details cleaning function
dd_prep = function(input_dd) {
  dd_prep <- input_dd %>%
    dplyr::mutate(Date = as.Date(Date, format="%m/%d/%Y"),
                  Sex = ifelse(Sex == "N", "U", Sex ),
                  Sex = ifelse(Sex == "Z", "U", Sex ),   # marking Z code as unknown for now
                  Sex = factor(Sex, levels = c("F", "M", "U", "O"), labels = c("Female", "Male", "Unknown", "Other")),
                  AgeGroup = case_when(             
                    AgeGroup == "00-04" ~ "0-4", 
                    AgeGroup == "05-17" ~ "5-17",
                    TRUE ~ AgeGroup
                  ),
                  AgeGroup = factor(AgeGroup, levels = c("0-4", "5-17", "18-44", "45-64", "65+", "Unknown")),
                  Race = dplyr::case_when(
                    c_race %in% c("Not Reported or Null", "Unknown", "Not Categorized", "Refused to answer") ~ "Unknown",
                    T ~ c_race
                  ),
                  Race =  factor(
                    Race,
                    levels = c(
                      'Asian',
                      'Black or African American',
                      'American Indian or Alaska Native',
                      'Native Hawaiian or Other Pacific Islander',
                      'White',
                      'Multiracial',
                      'Other Race',
                      'Unknown'
                    )
                  ),
                  Ethnicity = dplyr::case_when(
                    c_ethnicity %in% c("Not Reported or Null", "Unknown", "Not Categorized", "Refused to answer") ~ "Unknown",
                    T ~ c_ethnicity ),
                  Ethnicity = factor(
                    Ethnicity,
                    levels = c(
                      "Hispanic or Latino",
                      "Not Hispanic or Latino",
                      "Unknown"
                    )
                  )
    ) 
  
  
}

# TABLE PREP ----
# Each function inputs a data details dataframe
age_tbl = function(input_dd) {
  age_tbl <- input_dd %>%
    dplyr::group_by(AgeGroup, .drop=F) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    mutate(percentage = round((count / sum(count)) * 100, digits=1),
           Demographic = "AgeGroup") %>%
    rename(Group = AgeGroup) %>%
    select(Demographic, Group, count, percentage)
}

sex_tbl = function(input_dd) {
  sex_tbl <- input_dd %>%
    dplyr::group_by(Sex, .drop=F) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    mutate(percentage = round((count / sum(count)) * 100, digits=1),
           Demographic = "Sex") %>%
    rename(Group = Sex) %>%
    select(Demographic, Group, count, percentage) %>%
    filter(Group != "Other")
}

race_tbl = function(input_dd) {
  race_tbl <- input_dd %>%
    dplyr::group_by(Race, .drop=F) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    mutate(percentage = round((count / sum(count)) * 100, digits=1),
           Demographic = "Race") %>%
    rename(Group = Race) %>%
    select(Demographic, Group, count, percentage)
}

eth_tbl = function(input_dd) {
  eth_tbl <- input_dd %>%
    dplyr::group_by(Ethnicity, .drop=F) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    mutate(percentage = round((count / sum(count)) * 100, digits=1),
           Demographic = "Ethnicity") %>%
    rename(Group = Ethnicity) %>%
    select(Demographic, Group, count, percentage)
}




# Create a table of aggregated counts and percents by demographic group, with chi-square tests to compare observed counts for condition to expected counts if it had distribution of all-cause visits
# Takes numerator and denominator data details dataframes as input!! Runs data details cleaning and demographic aggregation functions within the function

create_flextable <- function(denomdf, numdf, title, includedenom = TRUE, analyze = TRUE){
  
  # Clean denominator data
  dd_denom_clean <- dd_prep(denomdf)
  
  # Aggregate denominator data by demographic
  denom_agetbl <- age_tbl(dd_denom_clean)
  denom_sextbl <- sex_tbl(dd_denom_clean)
  denom_racetbl <- race_tbl(dd_denom_clean)
  denom_ethtbl <- eth_tbl(dd_denom_clean)
  
  # Combine all the demographic aggregate tables to one overarching table
  combined_denomtbl <- bind_rows(denom_agetbl, denom_sextbl, denom_racetbl, denom_ethtbl)
  
  # Summarize n for denominator data (divide by 4 because it's summing across age, sex, race, and eth, really just need to sum one demographic group)
  denom_tot <- sum(combined_denomtbl$count)/4 
  
  # Clean condition data
  condition_clean <- dd_prep(numdf)
  
  # Aggregate condition data
  condition_agetbl <- age_tbl(condition_clean)
  condition_sextbl <- sex_tbl(condition_clean)
  condition_racetbl <- race_tbl(condition_clean)
  condition_ethtbl <- eth_tbl(condition_clean)
  
  # Combine all the demographic aggregate tables to one overarching table
  combined_conditiontbl <- bind_rows(condition_agetbl, condition_sextbl, condition_racetbl, condition_ethtbl)
  
  # Summarize n for numerator data
  num_tot <- sum(combined_conditiontbl$count)/4
  
  # Rename the condition the observed count
  numdf2 <- combined_conditiontbl %>%
    rename(observedcount = count,
           observedpercent = percentage)
  
  # Rename the denominator the all cause count
  denomdf2 <- combined_denomtbl %>%
    rename(allcausecount = count,
           allcausepercent = percentage) 
  
  # Join numerator and denominator data 
  df <- left_join(numdf2, denomdf2) 
  
  # Re-create expected visits column -- need unrounded proportion for chi-square to not error out. Cleaned percent fields for table won't work.
  df <- df %>%
    mutate(expected = allcausecount/denom_tot) 
  
  # Goodness of fit test function
  run_gof <- function(df, var){
    
    tbl <- df %>%
      filter(Demographic == var)
    
    test <- chisq.test(
      x = tbl$observedcount,  #observed needs to be a count - gets auto-converted to proportion
      p = tbl$expected # expected needs to be a proportion
    )
    
    tbl %>%
      mutate(
        residual = test$stdres,  #switch to "stdres" - the standardized pearson residual? Pearson residual is "residuals" field
        p_value = test$p.value
      )
    
  }
  
  dfnew <- bind_rows(run_gof(df, var = "AgeGroup"),
                     run_gof(df, var = "Sex"),
                     run_gof(df, var = "Race"),
                     run_gof(df, var = "Ethnicity"))
  
  # Format for table
  df <- dfnew %>%
    filter(Group != "Unknown") %>%  # filter out unknown group
    mutate(allcausecount = formatC(allcausecount, format = "d", big.mark = ","),
           `All ED Visits (%)` = paste0(allcausecount, " (", allcausepercent, "%)"),
           observedcount = formatC(observedcount, format = "d", big.mark = ","),
           countpercent = paste0(observedcount, " (", observedpercent, "%)")) %>%
    select(Demographic, Group, countpercent, `All ED Visits (%)`, p_value, residual )
  
  names(df)[names(df) == "countpercent"] <- paste(title,"ED Visits (%)")
  
  df_filter <- df %>%
    select(-residual, -p_value)
  
  set_flextable_defaults(
    font.size = 11, font.family = "Franklin Gothic Book",
    border.color = "#cfd3dd"
  )
  
  if(includedenom==FALSE){
    df_filter <- df_filter[, 1:3]
  } else {
    df_filter <- df_filter
  }
  
  finaltab <- flextable(df_filter) %>%  #need to be df_filter
        merge_v(j = "Demographic") %>%
        theme_vanilla() %>%
        bold(j = "Demographic", part = "body") %>%
        align(j = "Demographic", align = "center", part = "body") %>%
        #FitFlextableToPage(., pgwidth = 6.9) %>%
        bg(bg = "#203864", part = "header") %>%
        color(color = "white", part = "header") %>%
        padding(padding = 1, part = "all") %>%
        # Autofit for better display
        autofit() %>%
        align(align = "left", part = "footer") %>%
        fontsize(size = 8.25, part = "footer") %>%
        append_chunks(
          j = 3, part = "header",
          value = as_chunk(fmt_header_n(num_tot)))
  
if(analyze == TRUE){
  
  if(includedenom == TRUE){
    finaltab <- finaltab %>%
      append_chunks(j = 4, part = "header",
                    value = as_chunk(fmt_header_n(denom_tot))) %>%
      bold(i = abs(df$residual) >= 3 & df$p_value <0.01,  # Bold rows where overall chi-square test significant (p <0.01) and absolute value of residual > 3
           j = c(3,4),
           bold = TRUE,
           part = "body"
      ) %>%
      add_footer_lines(paste0("`Other` sex and `Unknown` values have been excluded from table. Statistically significant differences between ", title, " and all-cause visits are in bold.")) %>%
      autofit() 
  } else {
    
    finaltab <- finaltab %>%
      bold(i = abs(df$residual) >= 3 & df$p_value <0.01,  # Bold rows where overall chi-square test significant (p <0.01) and absolute value of residual > 3
           j = 3,
           bold = TRUE,
           part = "body"
      ) %>%
      add_footer_lines(paste0("`Other` sex and `Unknown` values have been excluded from table. Statistically significant differences between ", title, " and all-cause visits are in bold.")) %>%
      autofit() 
  }
  
} else { # if analyze is false
  if(includedenom == TRUE){
    finaltab <- finaltab %>%
      append_chunks(j = 4, part = "header",
                    value = as_chunk(fmt_header_n(denom_tot))) %>%
      add_footer_lines(paste0("`Other` sex and `Unknown` values have been excluded from table.")) %>%
      autofit()
  } else {
    finaltab <- finaltab %>%
      add_footer_lines(paste0("`Other` sex and `Unknown` values have been excluded from table.")) %>%
      autofit() 
  }
  
}
  
    
}

