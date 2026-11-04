# RHINO Seasonal Hazards Briefs
This repo contains Rmarkdown files that knit together reports for summer and winter conditions of interest alongside weather data. These reports include the same conditions seen on the public-facing Washington State [seasonal hazards dashboard](https://doh.wa.gov/public-health-provider-resources/healthcare-professions-and-facilities/data-exchange/syndromic-surveillance-rhino/seasonal-hazards-dashboard) and are designed to be used in supplement to provide more granular information during weather events. <b>Please note that because of the level of granularity, these reports are for internal purposes only and cannot be shared outside of public health.</b>

## Instructions:
1. Clone the repository. Click the green "Code" box dropdown in the righthand corner of the repository, and copy the web URL.
2. Open up a fresh RStudio session. Click File -> New Project -> Version Control -> Git, then paste the URL in the Repository URL box and click Create Project.
3. To run the summer report, open the HRI_Brief_LHJ.Rmd file. To run the winter report, open the CRI_Brief_LHJ.Rmd file.
4. Click the dropdown arrow next to Knit and select Knit with Parameters.
5. A UI will pop up with parameters. Select your county or LHJ, the data date range, and which data source you'd like to use (Patient Location or Facility Location). Click Knit to run.
6. The script will automatically generate a word doc report as output. If you want to make any changes to the report, for example updating the logo to that of your jurisdiction, you can do so by updating the word doc templates in the Support Resources file, then re-running the report.
