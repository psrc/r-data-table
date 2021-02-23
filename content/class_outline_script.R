# Script for the PSRC data.table class
# 02/23/2021
# Code from https://psrc.github.io/r-data-table/content/class_outline.html


############################
# Creating data.table object
############################
## -----------------------------------------------------------------------------------------------------------
library(data.table)
setwd("/Users/hana/psrc/rworkshops2020/data.table")


## -----------------------------------------------------------------------------------------------------------
### 1. Create it from scratch
d <- data.table(x = 1:10, y = letters[1:10])
d


## -----------------------------------------------------------------------------------------------------------
### 2. Convert existing data.frame
df <- data.frame(x = 1:10, y = letters[1:10])
d <- as.data.table(df)
d


## -----------------------------------------------------------------------------------------------------------
### 3. Coercion by reference
dft <- data.frame(x = 1:10, y = letters[1:10])
class(dft)
setDT(dft)
class(dft)


## -----------------------------------------------------------------------------------------------------------
### 4. Reading from file
dt <- fread("ofm_april1_population_final.csv")
class(dt)

str(dt)
# View(dt)


#############################
# Working with rows: DT[i,]
#############################

## Selecting rows
## -----------------------------------------------------------------------------------------------------------
dt[County == "King" & Filter < 4]

### Using base R function that returns a logical vector ------------------------------------------------------
dt[grepl("Wood", Jurisdiction)]

### Using indices --------------------------------------------------------------------------------------------
dt[1:3] # is equivalent to
dt[1:3,]

### Create a dataset of cities from our four counties -------------------------------------------------------- 
dt4c <- dt[Filter == 4 & County %chin% c("King", "Kitsap", "Pierce", "Snohomish")] 
dim(dt4c)


## Ordering rows
## -----------------------------------------------------------------------------------------------------------
dt4c[order(Jurisdiction)]

### order in place -------------------------------------------------------------------------------------------
setorder(dt4c, County, Jurisdiction)



###############################
# Working with columns: DT[,j]
###############################

## Renaming columns
## -----------------------------------------------------------------------------------------------------------
colnames(dt4c)

setnames(dt4c, "Jurisdiction", "City") # rename in place

colnames(dt4c)[5:ncol(dt4c)] <- substr(colnames(dt4c)[5:ncol(dt4c)], 1, 4)
colnames(dt4c)


## Selecting columns
### by column names --------------------------------------------------------------------------------------------
dt4c[, .(County, City)]

### by indices ------------------------------------------------------------------------------------------------
dt4c[1:10, 1:5]

### by ranges of columns --------------------------------------------------------------------------------------
dt4c[1:5, Filter:City]
dt4c[1:5, Filter:`2014`]

### the same as .() -------------------------------------------------------------------------------------------
dt4c[1:5, list(County, City)]

### as in data.frame
dt4c[1:5, c("County", "City")]

### but be aware if you store the names into an object --------------------------------------------------------
cols <- c("County", "City")
dt4c[, ..cols] # or
dt4c[, cols, with = FALSE]

### exclude columns ------------------------------------------------------------------------------------------
dt4c <- dt4c[, !c("Line", "Filter")]
dim(dt4c)


## Assigning values to columns
## -----------------------------------------------------------------------------------------------------------
dt4c[, id := 1:nrow(dt4c)]
dt4c[, id := id + 5000]


### be aware of implications of assigning by reference --------------------------------------------------------
d                 # d is our toy data.table
a <- d            # assign d to a
a[ , x := NULL][] # delete column in a
d                 # d changed as well!


### use copy() if you don't want the above behaviou ------------------------------------------------------------
d <- data.table(x = 1:10, y = letters[1:10]) # re-create d
a <- copy(d)      # use the copy() function to create a separate object
a[ , x := NULL][] # delete column in a
d                 # d is not impacted


## Partial assignment
## -----------------------------------------------------------------------------------------------------------
dt4c[County == "King", id := id + 1000]
dt4c[1:4, .(County, City, id)]
dt4c[50:53, .(County, City, id)]

### if column does not exist ---------------------------------------------------------------------------------
d[x < 5, z := TRUE][]

### removing a column ----------------------------------------------------------------------------------------
dt4c[, id := NULL]

## Multiple things at once
### assigning multiple columns ------------------------------------------------------------------------------
d[ , ':='(u = x^2, v = x^3, z = NULL)]

### chaining operations ---------------------------------------------------------------------------------------
d[, w := -4:5][w > 0, dummy := TRUE][order(x, decreasing = TRUE)]

### assigning multiple categories -----------------------------------------------------------------------------
d[, category := fcase(w > 0, "positive", 
                      w %between% c(-2, 0), "slightly negative",
                      w < -2, "negative")]

### another way of renaming columns ---------------------------------------------------------------------------
d[, dummy_new := dummy][, dummy := NULL][]


## Subsetting with .SD
### change the type of one column -----------------------------------------------------------------------------
dt4c[, `2010` := as.numeric(`2010`)]

### change the type of multiple columns -----------------------------------------------------------------------
cols <- as.character(2010:2019) # column names to apply the conversion to
dt4c[, (cols) := lapply(.SD, as.numeric), .SDcols = cols] 
str(dt4c)


#####################
# Group by: DT[,,by]
#####################
## Simple aggregations
## -----------------------------------------------------------------------------------------------------------
dt4c[, sum(`2019`)]
dt4c[, mean(`2019`)]

## special symbol for a count---------------------------------------------------------------------------------
dt4c[, .N]

# Group by
## -----------------------------------------------------------------------------------------------------------
dt4c[, sum(`2019`), by = County] # by a column
dt4c[, .(Population = sum(`2019`)), by = County] # with setting column names 
dt4c[, .(Population = sum(`2019`)), by = `2019` > 100000] # by expression
dt4c[, .(Population = sum(`2019`), Count = .N), by = `2019` > 100000] # adding a count
dt4c[, .(Population = sum(`2019`), Count = .N), by = .(County, `2019` > 100000)] # by column and expression


### group by result added as an additional column ------------------------------------------------------------
dt4c[, county_sum := sum(`2019`), by = County]


### using results of group by --------------------------------------------------------------------------------
dt4c[, county_share := round(`2019`/county_sum*100, 2)]
dt4c[, sum(county_share), by = County] 


## Using .SD
## -----------------------------------------------------------------------------------------------------------
dt4c[, lapply(.SD, sum), by = County, .SDcols = `2010`:`2019`]


###################
# Joining datasets
###################
## -----------------------------------------------------------------------------------------------------------
counties <- fread("counties.csv")

## The merge function
## -----------------------------------------------------------------------------------------------------------
dt4cm <- merge(dt4c, counties, by.x = "County", by.y = "county_name") # use "by" if joining columns are the same


## Join via the i dimension and setting keys
### first create another county dataset -----------------------------------------------------------------------
popcty <- dt4c[ , .(Pop = sum(`2019`)), by = County][, county_name := County][, County := NULL]

## -----------------------------------------------------------------------------------------------------------
popcty[counties, , on = .(county_name)] # left join
counties[popcty, , on = .(county_name)] # right join


### seting keys -----------------------------------------------------------------------------------------------
setkey(popcty, "county_name")
setkey(counties, "county_name")

### Then, no "on" argument needed------------------------------------------------------------------------------
counties[popcty]

### use "i." prefix to mark columns from the dataset inside the brackets --------------------------------------
counties[popcty, .(county_name, density = i.Pop/acres)]


### assign a new column via join ------------------------------------------------------------------------------
counties[popcty, density := i.Pop/acres][]


### notice that it is an outer join by default ----------------------------------------------------------------
popcty[county_name != "King"][counties]


### inner join ------------------------------------------------------------------------------------------------
popcty[county_name != "King"][counties, nomatch = NULL]


### "negation" join (gives records NOT in common) --------------------------------------------------------------
counties[!popcty[county_name != "King"]] 


#################
# Reshaping data
#################

### cleaning our dataset (removing columns) ------------------------------------------------------------------
head(dt4c)
dt4c[, ':='(county_sum = NULL, county_share = NULL)]


## Wide to long 
## -----------------------------------------------------------------------------------------------------------
dtl <- melt(dt4c, id.vars = c("County", "City"))
head(dtl)

### choosing appropriate column names -------------------------------------------------------------------------
dtl <- melt(dt4c, id.vars = c("County", "City"), variable.name = "Year", value.name = "Population")
head(dtl)


### Using "long" data in a ggplot graph -------------------------------------------------------------------------
library(ggplot2)
g <- ggplot(dtl[County == "Pierce"]) + 
      geom_col(aes(x = City, y = Population, fill = as.factor(Year)),
               position = "dodge") + coord_flip() + labs(fill = "Year")
print(g)

## Long to wide
## -----------------------------------------------------------------------------------------------------------
dtw <- dcast(dtl, County + City ~ Year, value.var = "Population")
dtw <- dcast(dtl, ... ~ Year, value.var = "Population")
head(dtw)


###################
# Exporting dataset
###################
fwrite(dtw, "my_ofm.csv")

