# Load packages
library(socialmixr)
library(data.table)
library(lubridate)
library(jsonlite)
library(countrycode)
library(ggplot2)

# Source custom functions
source("clean_survey.R")
source("load_survey2.R")

# Download CoMiX data
comix_survey <- download_survey("https://zenodo.org/records/13684044")

remove_X_col <- function(file){
    x <- fread(file)
    if ("X" %in% names(x)){
        x[,"X":=NULL]
    }
    fwrite(x, file = file)
}

change_country_name <- function(file, old_country, new_country){
    x <- fread(file)
    if ("country" %in% names(x)){
        x[country == old_country, country := new_country]
    }
    fwrite(x, file = file)
}

remove_X_col(comix_survey[3])
remove_X_col(comix_survey[5])
change_country_name(comix_survey[5], "uk", "GB")

# Load CoMix survey data
# comix <- load_survey(comix_survey, participant.age.column = "part_age") # doesn't work as part_age_exact is empty
comix <- load_survey2(comix_survey)

# Check part_age_exact is no longer empty
View(comix$participants) # it's not, it now has the lower age limit of the participant's age group

# Construct contact matrix
comix$participants[, date := as.Date(sday_id,"%Y.%m.%d")]
comix$participants[, Period := 0]
comix$participants[date >= as.Date("2020-03-23") & date <= as.Date("2020-06-03"), Period := 1]
cnt_mat <- contact_matrix(comix, filter = list(Period = 1), age.limits = c(0,5,12,18,30,40,50,60,70), symmetric = T)

# Plot contact matrix using ggplot
df <- reshape2::melt(cnt_mat$matrix, varnames = c("age.group", "age.group.contact"), value.name = "contacts")
ggplot(df, aes(x = age.group, y = age.group.contact, fill = contacts)) +
    xlab("Contactor age group") + 
    ylab("Contactee age group") + 
    theme(legend.position = "right") +
    geom_tile()
ggsave("comix_contact_matrix.pdf", width = 5.5, height = 4.4)
