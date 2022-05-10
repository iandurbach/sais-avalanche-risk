# csv files can be a pain to read into R because there are many ways to do this. 
# It is always a good idea to have a look at the csv file in (a) a spreadsheet program
# like Excel or Google Sheets, (b) a text editor. For example, here if you open up our csv 
# "glencoe_snowprofiles_2020-01-18_to_2022-03-18.csv" in a text editor you will see there are
# some "=" symbols that don't appear if you read the csv into Excel -- this causes trouble for R!!

# for example, read csv file into R using "default" way
x <- read.csv("data/Any_snowprofiles_2007-12-01_to_2022-05-02.csv")
# view it, see that the columns have been shifted wrongly, so the date is the row label, 
# and the column called "Date" contains the areas, etc. Wrong wrong wrong! 
View(x)
# here are the annoying "=" signs that should not be there, here in the Altitude variable
x$Alt[1:5]

# To do this a better way, we first find and remove all the "=" symbols in the csv file
# this is like doing a find and replace using a text editor
tx  <- readLines("data/Any_snowprofiles_2007-12-01_to_2022-05-02.csv")
tx2  <- gsub(pattern = "=", replace = "", x = tx)
# save the corrected csv as a new file
writeLines(tx2, con="output/fulldataset.csv")

# Now read into R using the "readr" package, this helps with some of the parsing.
library(readr)
x <- read_csv("output/fulldataset.csv")
x <- as.data.frame(x)
View(x) # check everything looks OK!

# make new variables OAH and FAH that contain observed and forecasted hazards
# just so we don't have to type the whole thing out each time
x$OAH <- x$`Observed aval. hazard`
x$FAH <- x$`Forecast aval. hazard`

# notice how the levels are ordered in alphabetic not "logical" order
# this is because the variable is stored as a type "character" variable
table(x$OAH)
class(x$OAH)

# recode these as factor variables, this tells R what the order of the levels is
x$OAH <- factor(x$OAH, levels = c("Low", "Moderate", "Considerable -", "Considerable +", "High"))
x$FAH <- factor(x$FAH, levels = c("Low", "Moderate", "Considerable -", "Considerable +", "High"))

# check again
table(x$OAH)
class(x$OAH)

# make a new variable containing "numeric" versions of the levels
# we use the "case_when" function from the dplyr package
library(dplyr)

x$OAH_num = case_when(
  x$OAH == "Low" ~ 1,
  x$OAH == "Moderate" ~ 2,
  x$OAH == "Considerable -" ~ 3,
  x$OAH == "Considerable +" ~ 4,
  x$OAH == "High" ~ 5,
)

# check again -- good idea to inspect data and check class ofter until you are familiar with
# these kinds of things in R
table(x$OAH_num)
class(x$OAH_num)

# do the same for the forecasted levels
x$FAH_num = case_when(
  x$FAH == "Low" ~ 1,
  x$FAH == "Moderate" ~ 2,
  x$FAH == "Considerable -" ~ 3,
  x$FAH == "Considerable +" ~ 4,
  x$FAH == "High" ~ 5,
)

table(x$FAH_num)
class(x$FAH_num)

saveRDS(x, file = "output/cleaned_fulldataset.Rds")

##### HERE IS SOME EXTRA STUFF JUST FOR INTEREST

# now can do some exploration of the dataset!

max(x$Date) # last reading
min(x$Date) # first reading

table(x$Area) # all from same area

hist(x$`Air Temp`) # distribution of air temps

# cross-table to check how often forecasts match observed levels
table(x$FAH, x$OAH)
# "correct" forecasts are along the diagonal
sum(diag(table(x$FAH, x$OAH)))
# total number of observations
sum(table(x$FAH, x$OAH))
# or just
nrow(x)
# proportion of correct forecasts
sum(diag(table(x$FAH, x$OAH))) / nrow(x)

# is there a relationship between air temp and forecasts?
x %>% group_by(FAH) %>% summarize(meanAT = mean(`Air Temp`),
                                  n = n())
# looks like forecasted hazards are higher when it is colder
plot(x$`Air Temp`, x$FAH) # can't really see much here because of the overlap (only 5 possible values of FAH)
# better plot, using ggplot
library(ggplot2)
ggplot(x, aes(x = `Air Temp`)) + geom_density(aes(colour = FAH))
# fit a simple regression, even though we know the response is discrete and non-normal
model0 <- lm(FAH_num ~ `Air Temp`, data = x)
summary(model0) # significant negative relationship between air temp and forecasts
