data <- read.table(header=TRUE, text='
 id weight
                   1     20
                   2     27
                   3     24
                   ')

# Ways to add a column
data$size      <- c("small", "large", "medium")
data[["size"]] <- c("small", "large", "medium")
data[,"size"]  <- c("small", "large", "medium")
data$size      <- 0   # Use the same value (0) for all rows


# Ways to remove the column
data$size      <- NULL
data[["size"]] <- NULL
data[,"size"]  <- NULL
data[[3]]      <- NULL
data[,3]       <- NULL
data           <- subset(data, select=-size)

# A sample data frame
data <- read.table(header=TRUE, text='
                   id weight   size
                   1     20  small
                   2     27  large
                   3     24 medium
                   ')

# Reorder by column number
data[c(1,3,2)]
#>   id   size weight
#> 1  1  small     20
#> 2  2  large     27
#> 3  3 medium     24

# To actually change `data`, you need to save it back into `data`:
# data <- data[c(1,3,2)]


# Reorder by column name
data[c("size", "id", "weight")]
#>     size id weight
#> 1  small  1     20
#> 2  large  2     27
#> 3 medium  3     24

# Matrix-Style Indexing data[row,col]
data[, c(1,3,2)]
#>   id   size weight
#> 1  1  small     20
#> 2  2  large     27
#> 3  3 medium     24

#Drawbacks
# List-style indexing of one column
data[2]
#>   weight
#> 1     20
#> 2     27
#> 3     24

# Matrix-style indexing of one column - drops dimension to become a vector
data[,2]
#> [1] 20 27 24

# Matrix-style indexing with drop=FALSE - preserves dimension to remain data frame
data[, 2, drop=FALSE]
#>   weight
#> 1     20
#> 2     27
#> 3     24

# Make a data frame mapping story numbers to titles
stories <- read.table(header=TRUE, text='
                      storyid  title
                      1       lions
                      2      tigers
                      3       bears
                      ')

# Make another data frame with the data and story numbers (no titles)
data <- read.table(header=TRUE, text='
                   subject storyid rating
                   1       1    6.7
                   1       2    4.5
                   1       3    3.7
                   2       2    3.3
                   2       3    4.1
                   2       1    5.2
                   ')

# Merge the two data frames
merge(stories, data, "storyid")
#>   storyid  title subject rating
#> 1       1  lions       1    6.7
#> 2       1  lions       2    5.2
#> 3       2 tigers       1    4.5
#> 4       2 tigers       2    3.3
#> 5       3  bears       1    3.7
#> 6       3  bears       2    4.1

# In this case, the column is named 'id' instead of storyid
stories2 <- read.table(header=TRUE, text='
                       id       title
                       1       lions
                       2      tigers
                       3       bears
                       ')

# Merge on stories2$id and data$storyid.
merge(x=stories2, y=data, by.x="id", by.y="storyid")
#>   id  title subject rating
#> 1  1  lions       1    6.7
#> 2  1  lions       2    5.2
#> 3  2 tigers       1    4.5
#> 4  2 tigers       2    3.3
#> 5  3  bears       1    3.7
#> 6  3  bears       2    4.1

# Note that the column name is inherited from the first data frame (x=stories2).

# Make up more data
animals <- read.table(header=T, text='
                      size type         name
                      small  cat         lynx
                      big  cat        tiger
                      small  dog    chihuahua
                      big  dog "great dane"
                      ')

observations <- read.table(header=T, text='
                           number  size type
                           1   big  cat
                           2 small  dog
                           3 small  dog
                           4   big  dog
                           ')

merge(observations, animals, c("size","type"))
#>    size type number       name
#> 1   big  cat      1      tiger
#> 2   big  dog      4 great dane
#> 3 small  dog      2  chihuahua
#> 4 small  dog      3  chihuahua

dfA <- data.frame(Subject=c(1,1,2,2), Response=c("X","X","X","X"))
dfA
#>   Subject Response
#> 1       1        X
#> 2       1        X
#> 3       2        X
#> 4       2        X

dfB <- data.frame(Subject=c(1,2,3), Response=c("X","Y","X"))
dfB
#>   Subject Response
#> 1       1        X
#> 2       2        Y
#> 3       3        X

dfC <- data.frame(Subject=c(1,2,3), Response=c("Z","Y","Z"))
dfC
#>   Subject Response
#> 1       1        Z
#> 2       2        Y
#> 3       3        Z

dfA$Coder <- "A"
dfB$Coder <- "B"
dfC$Coder <- "C"

df <- rbind(dfA, dfB, dfC)                    # Stick them together
df <- df[,c("Coder", "Subject", "Response")]  # Reorder the columns to look nice
df
#>    Coder Subject Response
#> 1      A       1        X
#> 2      A       1        X
#> 3      A       2        X
#> 4      A       2        X
#> 5      B       1        X
#> 6      B       2        Y
#> 7      B       3        X
#> 8      C       1        Z
#> 9      C       2        Y
#> 10     C       3        Z

# Find the rows which have duplicates in a different group.
dupRows <- dupsBetweenGroups(df, "Coder")

# Print it alongside the data frame
cbind(df, dup=dupRows)
#>    Coder Subject Response   dup
#> 1      A       1        X  TRUE
#> 2      A       1        X  TRUE
#> 3      A       2        X FALSE
#> 4      A       2        X FALSE
#> 5      B       1        X  TRUE
#> 6      B       2        Y  TRUE
#> 7      B       3        X FALSE
#> 8      C       1        Z FALSE
#> 9      C       2        Y  TRUE
#> 10     C       3        Z FALSE

cbind(df, unique=!dupRows)
#>    Coder Subject Response unique
#> 1      A       1        X  FALSE
#> 2      A       1        X  FALSE
#> 3      A       2        X   TRUE
#> 4      A       2        X   TRUE
#> 5      B       1        X  FALSE
#> 6      B       2        Y  FALSE
#> 7      B       3        X   TRUE
#> 8      C       1        Z   TRUE
#> 9      C       2        Y  FALSE
#> 10     C       3        Z   TRUE

# Store the results in df
dfDup <- cbind(df, dup=dupRows)

dfA <- subset(dfDup, Coder=="A", select=-Coder)
dfA
#>   Subject Response   dup
#> 1       1        X  TRUE
#> 2       1        X  TRUE
#> 3       2        X FALSE
#> 4       2        X FALSE

dfB <- subset(dfDup, Coder=="B", select=-Coder)
dfB
#>   Subject Response   dup
#> 5       1        X  TRUE
#> 6       2        Y  TRUE
#> 7       3        X FALSE

dfC <- subset(dfDup, Coder=="C", select=-Coder)
dfC
#>    Subject Response   dup
#> 8        1        Z FALSE
#> 9        2        Y  TRUE
#> 10       3        Z FALSE

# Ignore the Subject column -- only use Response
dfNoSub <- subset(df, select=-Subject)
dfNoSub
#>    Coder Response
#> 1      A        X
#> 2      A        X
#> 3      A        X
#> 4      A        X
#> 5      B        X
#> 6      B        Y
#> 7      B        X
#> 8      C        Z
#> 9      C        Y
#> 10     C        Z

# Check for duplicates
dupRows <- dupsBetweenGroups(dfNoSub, "Coder")

# Join the result to the original data frame
cbind(df, dup=dupRows)
#>    Coder Subject Response   dup
#> 1      A       1        X  TRUE
#> 2      A       1        X  TRUE
#> 3      A       2        X  TRUE
#> 4      A       2        X  TRUE
#> 5      B       1        X  TRUE
#> 6      B       2        Y  TRUE
#> 7      B       3        X  TRUE
#> 8      C       1        Z FALSE
#> 9      C       2        Y  TRUE
#> 10     C       3        Z FALSE

dupsBetweenGroups <- function (df, idcol) {
  # df: the data frame
  # idcol: the column which identifies the group each row belongs to
  
  # Get the data columns to use for finding matches
  datacols <- setdiff(names(df), idcol)
  
  # Sort by idcol, then datacols. Save order so we can undo the sorting later.
  sortorder <- do.call(order, df)
  df <- df[sortorder,]
  
  # Find duplicates within each id group (first copy not marked)
  dupWithin <- duplicated(df)
  
  # With duplicates within each group filtered out, find duplicates between groups. 
  # Need to scan up and down with duplicated() because first copy is not marked.
  dupBetween = rep(NA, nrow(df))
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols])
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols], fromLast=TRUE) | dupBetween[!dupWithin]
  
  # ============= Replace NA's with previous non-NA value ==============
  # This is why we sorted earlier - it was necessary to do this part efficiently
  
  # Get indexes of non-NA's
  goodIdx <- !is.na(dupBetween)
  
  # These are the non-NA values from x only
  # Add a leading NA for later use when we index into this vector
  goodVals <- c(NA, dupBetween[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  
  # The original vector, now with gaps filled
  dupBetween <- goodVals[fillIdx]
  
  # Undo the original sort
  dupBetween[sortorder] <- dupBetween
  
  # Return the vector of which entries are duplicated across groups
  return(dupBetween)
}

d <- read.csv(header = TRUE, text='
x,y,value
              a,one,1
              ,,5
              b,two,4
              c,three,10
              ')

d
#>   x     y value
#> 1 a   one     1
#> 2             5
#> 3 b   two     4
#> 4 c three    10

str(d)
#> 'data.frame':	4 obs. of  3 variables:
#>  $ x    : Factor w/ 4 levels "","a","b","c": 2 1 3 4
#>  $ y    : Factor w/ 4 levels "","one","three",..: 2 1 4 3
#>  $ value: int  1 5 4 10

# Remove second row
d <- d[-2,]
d
#>   x     y value
#> 1 a   one     1
#> 3 b   two     4
#> 4 c three    10

str(d)
#> 'data.frame':	3 obs. of  3 variables:
#>  $ x    : Factor w/ 4 levels "","a","b","c": 2 3 4
#>  $ y    : Factor w/ 4 levels "","one","three",..: 2 4 3
#>  $ value: int  1 4 10

d1 <- droplevels(d)
str(d1)
#> 'data.frame':	3 obs. of  3 variables:
#>  $ x    : Factor w/ 3 levels "a","b","c": 1 2 3
#>  $ y    : Factor w/ 3 levels "one","three",..: 1 3 2
#>  $ value: int  1 4 10

# Find which columns are factors
factor_cols <- vapply(d, is.factor, logical(1))

# Apply the factor() function to those columns, and assign then back into d
d[factor_cols] <- lapply(d[factor_cols], factor)
str(d)
#> 'data.frame':	3 obs. of  3 variables:
#>  $ x    : Factor w/ 3 levels "a","b","c": 1 2 3
#>  $ y    : Factor w/ 3 levels "one","three",..: 1 3 2
#>  $ value: int  1 4 10

olddata_wide <- read.table(header=TRUE, text='
 subject sex control cond1 cond2
                           1   M     7.9  12.3  10.7
                           2   F     6.3  10.6  11.1
                           3   F     9.5  13.1  13.8
                           4   M    11.5  13.4  12.9
                           ')
# Make sure the subject column is a factor
olddata_wide$subject <- factor(olddata_wide$subject)

olddata_long <- read.table(header=TRUE, text='
 subject sex condition measurement
                           1   M   control         7.9
                           1   M     cond1        12.3
                           1   M     cond2        10.7
                           2   F   control         6.3
                           2   F     cond1        10.6
                           2   F     cond2        11.1
                           3   F   control         9.5
                           3   F     cond1        13.1
                           3   F     cond2        13.8
                           4   M   control        11.5
                           4   M     cond1        13.4
                           4   M     cond2        12.9
                           ')
# Make sure the subject column is a factor
olddata_long$subject <- factor(olddata_long$subject)

olddata_wide
#>   subject sex control cond1 cond2
#> 1       1   M     7.9  12.3  10.7
#> 2       2   F     6.3  10.6  11.1
#> 3       3   F     9.5  13.1  13.8
#> 4       4   M    11.5  13.4  12.9

library(tidyr)

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
data_long <- gather(olddata_wide, condition, measurement, control:cond2, factor_key=TRUE)
data_long
#>    subject sex condition measurement
#> 1        1   M   control         7.9
#> 2        2   F   control         6.3
#> 3        3   F   control         9.5
#> 4        4   M   control        11.5
#> 5        1   M     cond1        12.3
#> 6        2   F     cond1        10.6
#> 7        3   F     cond1        13.1
#> 8        4   M     cond1        13.4
#> 9        1   M     cond2        10.7
#> 10       2   F     cond2        11.1
#> 11       3   F     cond2        13.8
#> 12       4   M     cond2        12.9

gather(olddata_wide, condition, measurement, control, cond1, cond2)

keycol <- "condition"
valuecol <- "measurement"
gathercols <- c("control", "cond1", "cond2")

gather_(olddata_wide, keycol, valuecol, gathercols)

# Rename factor names from "cond1" and "cond2" to "first" and "second"
levels(data_long$condition)[levels(data_long$condition)=="cond1"] <- "first"
levels(data_long$condition)[levels(data_long$condition)=="cond2"] <- "second"

# Sort by subject first, then by condition
data_long <- data_long[order(data_long$subject, data_long$condition), ]
data_long
#>    subject sex condition measurement
#> 1        1   M   control         7.9
#> 5        1   M     first        12.3
#> 9        1   M    second        10.7
#> 2        2   F   control         6.3
#> 6        2   F     first        10.6
#> 10       2   F    second        11.1
#> 3        3   F   control         9.5
#> 7        3   F     first        13.1
#> 11       3   F    second        13.8
#> 4        4   M   control        11.5
#> 8        4   M     first        13.4
#> 12       4   M    second        12.9

olddata_long
#>    subject sex condition measurement
#> 1        1   M   control         7.9
#> 2        1   M     cond1        12.3
#> 3        1   M     cond2        10.7
#> 4        2   F   control         6.3
#> 5        2   F     cond1        10.6
#> 6        2   F     cond2        11.1
#> 7        3   F   control         9.5
#> 8        3   F     cond1        13.1
#> 9        3   F     cond2        13.8
#> 10       4   M   control        11.5
#> 11       4   M     cond1        13.4
#> 12       4   M     cond2        12.9

library(tidyr)

# The arguments to spread():
# - data: Data object
# - key: Name of column containing the new column names
# - value: Name of column containing values
data_wide <- spread(olddata_long, condition, measurement)
data_wide
#>   subject sex cond1 cond2 control
#> 1       1   M  12.3  10.7     7.9
#> 2       2   F  10.6  11.1     6.3
#> 3       3   F  13.1  13.8     9.5
#> 4       4   M  13.4  12.9    11.5

# Rename cond1 to first, and cond2 to second
names(data_wide)[names(data_wide)=="cond1"] <- "first"
names(data_wide)[names(data_wide)=="cond2"] <- "second"

# Reorder the columns
data_wide <- data_wide[, c(1,2,5,3,4)]
data_wide
#>   subject sex control first second
#> 1       1   M     7.9  12.3   10.7
#> 2       2   F     6.3  10.6   11.1
#> 3       3   F     9.5  13.1   13.8
#> 4       4   M    11.5  13.4   12.9

olddata_wide
#>   subject sex control cond1 cond2
#> 1       1   M     7.9  12.3  10.7
#> 2       2   F     6.3  10.6  11.1
#> 3       3   F     9.5  13.1  13.8
#> 4       4   M    11.5  13.4  12.9

library(reshape2)

# Specify id.vars: the variables to keep but not split apart on
melt(olddata_wide, id.vars=c("subject", "sex"))
#>    subject sex variable value
#> 1        1   M  control   7.9
#> 2        2   F  control   6.3
#> 3        3   F  control   9.5
#> 4        4   M  control  11.5
#> 5        1   M    cond1  12.3
#> 6        2   F    cond1  10.6
#> 7        3   F    cond1  13.1
#> 8        4   M    cond1  13.4
#> 9        1   M    cond2  10.7
#> 10       2   F    cond2  11.1
#> 11       3   F    cond2  13.8
#> 12       4   M    cond2  12.9

data_long <- melt(olddata_wide,
                  # ID variables - all the variables to keep but not split apart on
                  id.vars=c("subject", "sex"),
                  # The source columns
                  measure.vars=c("control", "cond1", "cond2" ),
                  # Name of the destination column that will identify the original
                  # column that the measurement came from
                  variable.name="condition",
                  value.name="measurement"
)
data_long
#>    subject sex condition measurement
#> 1        1   M   control         7.9
#> 2        2   F   control         6.3
#> 3        3   F   control         9.5
#> 4        4   M   control        11.5
#> 5        1   M     cond1        12.3
#> 6        2   F     cond1        10.6
#> 7        3   F     cond1        13.1
#> 8        4   M     cond1        13.4
#> 9        1   M     cond2        10.7
#> 10       2   F     cond2        11.1
#> 11       3   F     cond2        13.8
#> 12       4   M     cond2        12.9

# Rename factor names from "cond1" and "cond2" to "first" and "second"
levels(data_long$condition)[levels(data_long$condition)=="cond1"] <- "first"
levels(data_long$condition)[levels(data_long$condition)=="cond2"] <- "second"

# Sort by subject first, then by condition
data_long <- data_long[ order(data_long$subject, data_long$condition), ]
data_long
#>    subject sex condition measurement
#> 1        1   M   control         7.9
#> 5        1   M     first        12.3
#> 9        1   M    second        10.7
#> 2        2   F   control         6.3
#> 6        2   F     first        10.6
#> 10       2   F    second        11.1
#> 3        3   F   control         9.5
#> 7        3   F     first        13.1
#> 11       3   F    second        13.8
#> 4        4   M   control        11.5
#> 8        4   M     first        13.4
#> 12       4   M    second        12.9

olddata_long
#>    subject sex condition measurement
#> 1        1   M   control         7.9
#> 2        1   M     cond1        12.3
#> 3        1   M     cond2        10.7
#> 4        2   F   control         6.3
#> 5        2   F     cond1        10.6
#> 6        2   F     cond2        11.1
#> 7        3   F   control         9.5
#> 8        3   F     cond1        13.1
#> 9        3   F     cond2        13.8
#> 10       4   M   control        11.5
#> 11       4   M     cond1        13.4
#> 12       4   M     cond2        12.9

# From the source:
# "subject" and "sex" are columns we want to keep the same
# "condition" is the column that contains the names of the new column to put things in
# "measurement" holds the measurements
library(reshape2)

data_wide <- dcast(olddata_long, subject + sex ~ condition, value.var="measurement")
data_wide
#>   subject sex cond1 cond2 control
#> 1       1   M  12.3  10.7     7.9
#> 2       2   F  10.6  11.1     6.3
#> 3       3   F  13.1  13.8     9.5
#> 4       4   M  13.4  12.9    11.5

# Rename cond1 to first, and cond2 to second
names(data_wide)[names(data_wide)=="cond1"] <- "first"
names(data_wide)[names(data_wide)=="cond2"] <- "second"

# Reorder the columns
data_wide <- data_wide[, c(1,2,5,3,4)]
data_wide
#>   subject sex control first second
#> 1       1   M     7.9  12.3   10.7
#> 2       2   F     6.3  10.6   11.1
#> 3       3   F     9.5  13.1   13.8
#> 4       4   M    11.5  13.4   12.9

data <- read.table(header=TRUE, text='
 subject sex condition before after change
                   1   F   placebo   10.1   6.9   -3.2
                   2   F   placebo    6.3   4.2   -2.1
                   3   M   aspirin   12.4   6.3   -6.1
                   4   F   placebo    8.1   6.1   -2.0
                   5   M   aspirin   15.2   9.9   -5.3
                   6   F   aspirin   10.9   7.0   -3.9
                   7   F   aspirin   11.6   8.5   -3.1
                   8   M   aspirin    9.5   3.0   -6.5
                   9   F   placebo   11.5   9.0   -2.5
                   10   M   placebo   11.9  11.0   -0.9
                   11   F   aspirin   11.4   8.0   -3.4
                   12   M   aspirin   10.0   4.4   -5.6
                   13   M   aspirin   12.5   5.4   -7.1
                   14   M   placebo   10.6  10.6    0.0
                   15   M   aspirin    9.1   4.3   -4.8
                   16   F   placebo   12.1  10.2   -1.9
                   17   F   placebo   11.0   8.8   -2.2
                   18   F   placebo   11.9  10.2   -1.7
                   19   M   aspirin    9.1   3.6   -5.5
                   20   M   placebo   13.5  12.4   -1.1
                   21   M   aspirin   12.0   7.5   -4.5
                   22   F   placebo    9.1   7.6   -1.5
                   23   M   placebo    9.9   8.0   -1.9
                   24   F   placebo    7.6   5.2   -2.4
                   25   F   placebo   11.8   9.7   -2.1
                   26   F   placebo   11.8  10.7   -1.1
                   27   F   aspirin   10.1   7.9   -2.2
                   28   M   aspirin   11.6   8.3   -3.3
                   29   F   aspirin   11.3   6.8   -4.5
                   30   F   placebo   10.3   8.3   -2.0
                   ')

library(plyr)

# Run the functions length, mean, and sd on the value of "change" for each group, 
# broken down by sex + condition
cdata <- ddply(data, c("sex", "condition"), summarise,
               N    = length(change),
               mean = mean(change),
               sd   = sd(change),
               se   = sd / sqrt(N)
)
cdata
#>   sex condition  N      mean        sd        se
#> 1   F   aspirin  5 -3.420000 0.8642916 0.3865230
#> 2   F   placebo 12 -2.058333 0.5247655 0.1514867
#> 3   M   aspirin  9 -5.411111 1.1307569 0.3769190
#> 4   M   placebo  4 -0.975000 0.7804913 0.3902456

# Put some NA's in the data
dataNA <- data
dataNA$change[11:14] <- NA

cdata <- ddply(dataNA, c("sex", "condition"), summarise,
               N    = sum(!is.na(change)),
               mean = mean(change, na.rm=TRUE),
               sd   = sd(change, na.rm=TRUE),
               se   = sd / sqrt(N)
)
cdata
#>   sex condition  N      mean        sd        se
#> 1   F   aspirin  4 -3.425000 0.9979145 0.4989572
#> 2   F   placebo 12 -2.058333 0.5247655 0.1514867
#> 3   M   aspirin  7 -5.142857 1.0674848 0.4034713
#> 4   M   placebo  3 -1.300000 0.5291503 0.3055050

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

summarySE(data, measurevar="change", groupvars=c("sex", "condition"))
#>   sex condition  N    change        sd        se        ci
#> 1   F   aspirin  5 -3.420000 0.8642916 0.3865230 1.0731598
#> 2   F   placebo 12 -2.058333 0.5247655 0.1514867 0.3334201
#> 3   M   aspirin  9 -5.411111 1.1307569 0.3769190 0.8691767
#> 4   M   placebo  4 -0.975000 0.7804913 0.3902456 1.2419358

# With a data set with NA's, use na.rm=TRUE
summarySE(dataNA, measurevar="change", groupvars=c("sex", "condition"), na.rm=TRUE)
#>   sex condition  N    change        sd        se        ci
#> 1   F   aspirin  4 -3.425000 0.9979145 0.4989572 1.5879046
#> 2   F   placebo 12 -2.058333 0.5247655 0.1514867 0.3334201
#> 3   M   aspirin  7 -5.142857 1.0674848 0.4034713 0.9872588
#> 4   M   placebo  3 -1.300000 0.5291503 0.3055050 1.3144821

# First remove some all Male+Placebo entries from the data
dataSub <- subset(data, !(sex=="M" & condition=="placebo"))

# If we summarize the data, there will be a missing row for Male+Placebo,
# since there were no cases with this combination.
summarySE(dataSub, measurevar="change", groupvars=c("sex", "condition"))
#>   sex condition  N    change        sd        se        ci
#> 1   F   aspirin  5 -3.420000 0.8642916 0.3865230 1.0731598
#> 2   F   placebo 12 -2.058333 0.5247655 0.1514867 0.3334201
#> 3   M   aspirin  9 -5.411111 1.1307569 0.3769190 0.8691767

# Set .drop=FALSE to NOT drop those combinations
summarySE(dataSub, measurevar="change", groupvars=c("sex", "condition"), .drop=FALSE)
#> Warning in qt(conf.interval/2 + 0.5, datac$N - 1): NaNs produced
#>   sex condition  N    change        sd        se        ci
#> 1   F   aspirin  5 -3.420000 0.8642916 0.3865230 1.0731598
#> 2   F   placebo 12 -2.058333 0.5247655 0.1514867 0.3334201
#> 3   M   aspirin  9 -5.411111 1.1307569 0.3769190 0.8691767
#> 4   M   placebo  0       NaN        NA        NA        NA

library(doBy)

# Run the functions length, mean, and sd on the value of "change" for each group, 
# broken down by sex + condition
cdata <- summaryBy(change ~ sex + condition, data=data, FUN=c(length,mean,sd))
cdata
#>   sex condition change.length change.mean change.sd
#> 1   F   aspirin             5   -3.420000 0.8642916
#> 2   F   placebo            12   -2.058333 0.5247655
#> 3   M   aspirin             9   -5.411111 1.1307569
#> 4   M   placebo             4   -0.975000 0.7804913

# Rename column change.length to just N
names(cdata)[names(cdata)=="change.length"] <- "N"

# Calculate standard error of the mean
cdata$change.se <- cdata$change.sd / sqrt(cdata$N)
cdata
#>   sex condition  N change.mean change.sd change.se
#> 1   F   aspirin  5   -3.420000 0.8642916 0.3865230
#> 2   F   placebo 12   -2.058333 0.5247655 0.1514867
#> 3   M   aspirin  9   -5.411111 1.1307569 0.3769190
#> 4   M   placebo  4   -0.975000 0.7804913 0.3902456

# New version of length which can handle NA's: if na.rm==T, don't count them
length2 <- function (x, na.rm=FALSE) {
  if (na.rm) sum(!is.na(x))
  else       length(x)
}

# Put some NA's in the data
dataNA <- data
dataNA$change[11:14] <- NA

cdataNA <- summaryBy(change ~ sex + condition, data=dataNA,
                     FUN=c(length2, mean, sd), na.rm=TRUE)
cdataNA
#>   sex condition change.length2 change.mean change.sd
#> 1   F   aspirin              4   -3.425000 0.9979145
#> 2   F   placebo             12   -2.058333 0.5247655
#> 3   M   aspirin              7   -5.142857 1.0674848
#> 4   M   placebo              3   -1.300000 0.5291503

# Now, do the same as before

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence 
## interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
  library(doBy)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

summarySE(data, measurevar="change", groupvars=c("sex","condition"))
#>   sex condition  N    change        sd        se        ci
#> 1   F   aspirin  5 -3.420000 0.8642916 0.3865230 1.0731598
#> 2   F   placebo 12 -2.058333 0.5247655 0.1514867 0.3334201
#> 3   M   aspirin  9 -5.411111 1.1307569 0.3769190 0.8691767
#> 4   M   placebo  4 -0.975000 0.7804913 0.3902456 1.2419358

# With a data set with NA's, use na.rm=TRUE
summarySE(dataNA, measurevar="change", groupvars=c("sex","condition"), na.rm=TRUE)
#>   sex condition  N    change        sd        se        ci
#> 1   F   aspirin  4 -3.425000 0.9979145 0.4989572 1.5879046
#> 2   F   placebo 12 -2.058333 0.5247655 0.1514867 0.3334201
#> 3   M   aspirin  7 -5.142857 1.0674848 0.4034713 0.9872588
#> 4   M   placebo  3 -1.300000 0.5291503 0.3055050 1.3144821

fillMissingCombs <- function(df, factors, measures) {
  
  # Make a list of the combinations of factor levels
  levelList <- list()
  for (f in factors) {  levelList[[f]] <- levels(df[,f])  }
  
  fullFactors <- expand.grid(levelList)
  
  dfFull <- merge(fullFactors, df, all.x=TRUE)
  
  # Wherever there is an NA in the measure vars, replace with 0
  for (m in measures) {
    dfFull[is.na(dfFull[,m]), m] <- 0
  }
  
  return(dfFull)
}

# First remove some all Male+Placebo entries from the data
dataSub <- subset(data, !(sex=="M" & condition=="placebo"))

# If we summarize the data, there will be a missing row for Male+Placebo,
# since there were no cases with this combination.
cdataSub <- summarySE(dataSub, measurevar="change", groupvars=c("sex", "condition"))
cdataSub
#>   sex condition  N    change        sd        se        ci
#> 1   F   aspirin  5 -3.420000 0.8642916 0.3865230 1.0731598
#> 2   F   placebo 12 -2.058333 0.5247655 0.1514867 0.3334201
#> 3   M   aspirin  9 -5.411111 1.1307569 0.3769190 0.8691767

# This will fill in the missing combinations with zeros
fillMissingCombs(cdataSub, factors=c("sex","condition"), measures=c("N","change","sd","se","ci"))
#>   sex condition  N    change        sd        se        ci
#> 1   F   aspirin  5 -3.420000 0.8642916 0.3865230 1.0731598
#> 2   F   placebo 12 -2.058333 0.5247655 0.1514867 0.3334201
#> 3   M   aspirin  9 -5.411111 1.1307569 0.3769190 0.8691767
#> 4   M   placebo  0  0.000000 0.0000000 0.0000000 0.0000000

# Get a count of number of subjects in each category (sex*condition)
cdata <- aggregate(data["subject"], by=data[c("sex","condition")], FUN=length)
cdata
#>   sex condition subject
#> 1   F   aspirin       5
#> 2   M   aspirin       9
#> 3   F   placebo      12
#> 4   M   placebo       4

# Rename "subject" column to "N"
names(cdata)[names(cdata)=="subject"] <- "N"
cdata
#>   sex condition  N
#> 1   F   aspirin  5
#> 2   M   aspirin  9
#> 3   F   placebo 12
#> 4   M   placebo  4

# Sort by sex first
cdata <- cdata[order(cdata$sex),]
cdata
#>   sex condition  N
#> 1   F   aspirin  5
#> 3   F   placebo 12
#> 2   M   aspirin  9
#> 4   M   placebo  4

# We also keep the __before__ and __after__ columns:
# Get the average effect size by sex and condition
cdata.means <- aggregate(data[c("before","after","change")], 
                         by = data[c("sex","condition")], FUN=mean)
cdata.means
#>   sex condition   before     after    change
#> 1   F   aspirin 11.06000  7.640000 -3.420000
#> 2   M   aspirin 11.26667  5.855556 -5.411111
#> 3   F   placebo 10.13333  8.075000 -2.058333
#> 4   M   placebo 11.47500 10.500000 -0.975000

# Merge the data frames
cdata <- merge(cdata, cdata.means)
cdata
#>   sex condition  N   before     after    change
#> 1   F   aspirin  5 11.06000  7.640000 -3.420000
#> 2   F   placebo 12 10.13333  8.075000 -2.058333
#> 3   M   aspirin  9 11.26667  5.855556 -5.411111
#> 4   M   placebo  4 11.47500 10.500000 -0.975000

# Get the sample (n-1) standard deviation for "change"
cdata.sd <- aggregate(data["change"],
                      by = data[c("sex","condition")], FUN=sd)
# Rename the column to change.sd
names(cdata.sd)[names(cdata.sd)=="change"] <- "change.sd"
cdata.sd
#>   sex condition change.sd
#> 1   F   aspirin 0.8642916
#> 2   M   aspirin 1.1307569
#> 3   F   placebo 0.5247655
#> 4   M   placebo 0.7804913

# Merge
cdata <- merge(cdata, cdata.sd)
cdata
#>   sex condition  N   before     after    change change.sd
#> 1   F   aspirin  5 11.06000  7.640000 -3.420000 0.8642916
#> 2   F   placebo 12 10.13333  8.075000 -2.058333 0.5247655
#> 3   M   aspirin  9 11.26667  5.855556 -5.411111 1.1307569
#> 4   M   placebo  4 11.47500 10.500000 -0.975000 0.7804913

# Calculate standard error of the mean
cdata$change.se <- cdata$change.sd / sqrt(cdata$N)
cdata
#>   sex condition  N   before     after    change change.sd change.se
#> 1   F   aspirin  5 11.06000  7.640000 -3.420000 0.8642916 0.3865230
#> 2   F   placebo 12 10.13333  8.075000 -2.058333 0.5247655 0.1514867
#> 3   M   aspirin  9 11.26667  5.855556 -5.411111 1.1307569 0.3769190
#> 4   M   placebo  4 11.47500 10.500000 -0.975000 0.7804913 0.3902456

cdata.means <- aggregate(data[c("before","after","change")], 
                         by = data[c("sex","condition")],
                         FUN=mean, na.rm=TRUE)
cdata.means
#>   sex condition   before     after    change
#> 1   F   aspirin 11.06000  7.640000 -3.420000
#> 2   M   aspirin 11.26667  5.855556 -5.411111
#> 3   F   placebo 10.13333  8.075000 -2.058333
#> 4   M   placebo 11.47500 10.500000 -0.975000

# Each row represents one case
cases <- data.frame(
  Sex=c("M", "M", "F", "F", "F"), 
  Color=c("brown", "blue", "brown", "brown", "brown")
)
cases
#>   Sex Color
#> 1   M brown
#> 2   M  blue
#> 3   F brown
#> 4   F brown
#> 5   F brown

# A contingency table
ctable <- table(cases)
ctable
#>    Color
#> Sex blue brown
#>   F    0     3
#>   M    1     1

# A table with counts of each combination
counts <- data.frame(
  Sex=c("F", "M", "F", "M"), 
  Color=c("blue", "blue", "brown", "brown"),
  Freq=c(0, 1, 3, 1)
)
counts
#>   Sex Color Freq
#> 1   F  blue    0
#> 2   M  blue    1
#> 3   F brown    3
#> 4   M brown    1

# Cases to Table
ctable  <- table(cases)
ctable
#>    Color
#> Sex blue brown
#>   F    0     3
#>   M    1     1

# If you call table using two vectors, it will not add names (Sex and Color) to
# the dimensions.
table(cases$Sex, cases$Color)
#>    
#>     blue brown
#>   F    0     3
#>   M    1     1

# The dimension names can be specified manually with `dnn`, or by using a subset
# of the data frame that contains only the desired columns.
table(cases$Sex, cases$Color, dnn=c("Sex","Color"))
#>    Color
#> Sex blue brown
#>   F    0     3
#>   M    1     1
table(cases[,c("Sex","Color")])
#>    Color
#> Sex blue brown
#>   F    0     3
#>   M    1     1

# Cases to Counts
countdf <- as.data.frame(table(cases))
countdf
#>   Sex Color Freq
#> 1   F  blue    0
#> 2   M  blue    1
#> 3   F brown    3
#> 4   M brown    1

countsToCases(as.data.frame(ctable))
#>     Sex Color
#> 2     M  blue
#> 3     F brown
#> 3.1   F brown
#> 3.2   F brown
#> 4     M brown

as.data.frame(ctable)
#>   Sex Color Freq
#> 1   F  blue    0
#> 2   M  blue    1
#> 3   F brown    3
#> 4   M brown    1

countsToCases(countdf)
#>     Sex Color
#> 2     M  blue
#> 3     F brown
#> 3.1   F brown
#> 3.2   F brown
#> 4     M brown

xtabs(Freq ~ Sex+Color, data=countdf)
#>    Color
#> Sex blue brown
#>   F    0     3
#>   M    1     1

# Convert from data frame of counts to data frame of cases.
# `countcol` is the name of the column containing the counts
countsToCases <- function(x, countcol = "Freq") {
  # Get the row indices to pull from x
  idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
  
  # Drop count column
  x[[countcol]] <- NULL
  
  # Get the rows from x
  x[idx, ]
}

set.seed(993)
x <- 1:300
y <- sin(x/20) + rnorm(300,sd=.1)
y[251:255] <- NA

# Plot the unsmoothed data (gray)
plot(x, y, type="l", col=grey(.5))
# Draw gridlines
grid()

# Smoothed with lag:
# average of current sample and 19 previous samples (red)
f20 <- rep(1/20, 20)
f20
#>  [1] 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05
#> [18] 0.05 0.05 0.05
y_lag <- filter(y, f20, sides=1)
lines(x, y_lag, col="red")

# Smoothed symmetrically:
# average of current sample, 10 future samples, and 10 past samples (blue)
f21 <- rep(1/21,21)
f21
#>  [1] 0.04761905 0.04761905 0.04761905 0.04761905 0.04761905 0.04761905 0.04761905
#>  [8] 0.04761905 0.04761905 0.04761905 0.04761905 0.04761905 0.04761905 0.04761905
#> [15] 0.04761905 0.04761905 0.04761905 0.04761905 0.04761905 0.04761905 0.04761905
y_sym <- filter(y, f21, sides=2)
lines(x, y_sym, col="blue")

# x: the vector
# n: the number of samples
# centered: if FALSE, then average current sample and previous (n-1) samples
#           if TRUE, then average symmetrically in past and future. (If n is even, use one more sample from future.)
movingAverage <- function(x, n=1, centered=FALSE) {
  
  if (centered) {
    before <- floor  ((n-1)/2)
    after  <- ceiling((n-1)/2)
  } else {
    before <- n-1
    after  <- 0
  }
  
  # Track the sum and count of number of non-NA items
  s     <- rep(0, length(x))
  count <- rep(0, length(x))
  
  # Add the centered data 
  new <- x
  # Add to count list wherever there isn't a 
  count <- count + !is.na(new)
  # Now replace NA_s with 0_s and add to total
  new[is.na(new)] <- 0
  s <- s + new
  
  # Add the data from before
  i <- 1
  while (i <= before) {
    # This is the vector with offset values to add
    new   <- c(rep(NA, i), x[1:(length(x)-i)])
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i+1
  }
  
  # Add the data from after
  i <- 1
  while (i <= after) {
    # This is the vector with offset values to add
    new   <- c(x[(i+1):length(x)], rep(NA, i))
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i+1
  }
  
  # return sum divided by count
  s/count
}


# Make same plots from before, with thicker lines
plot(x, y, type="l", col=grey(.5))
grid()
y_lag <- filter(y, rep(1/20, 20), sides=1)
lines(x, y_lag, col="red", lwd=4)         # Lagged average in red
y_sym <- filter(y, rep(1/21,21), sides=2)
lines(x, y_sym, col="blue", lwd=4)        # Symmetrical average in blue

# Calculate lagged moving average with new method and overplot with green
y_lag_na.rm <- movingAverage(y, 20)
lines(x, y_lag_na.rm, col="green", lwd=2)

# Calculate symmetrical moving average  with new method and overplot with green
y_sym_na.rm <- movingAverage(y, 21, TRUE)
lines(x, y_sym_na.rm, col="green", lwd=2)



# Generate a vector with 22 random numbers from 0-99
set.seed(123)
x <- floor(runif(22)*100)
x
#>  [1] 28 78 40 88 94  4 52 89 55 45 95 45 67 57 10 89 24  4 32 95 88 69

# Round up the length of vector the to the nearest 4
newlength <- ceiling(length(x)/4)*4
newlength
#> [1] 24

# Pad x with NA's up to the new length
x[newlength] <- NA
x
#>  [1] 28 78 40 88 94  4 52 89 55 45 95 45 67 57 10 89 24  4 32 95 88 69 NA NA

# Convert to a matrix with 4 rows
x <- matrix(x, nrow=4)
x
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]   28   94   55   67   24   88
#> [2,]   78    4   45   57    4   69
#> [3,]   40   52   95   10   32   NA
#> [4,]   88   89   45   89   95   NA

# Take the means of the columns, and ignore any NA's
colMeans(x, na.rm=TRUE)
#> [1] 58.50 59.75 60.00 55.75 38.75 78.50

# Example data
v <- c("A","A","A", "B","B","B","B", NA,NA, "C","C", "B", "C","C","C")
v
#>  [1] "A" "A" "A" "B" "B" "B" "B" NA  NA  "C" "C" "B" "C" "C" "C"

vr <- rle(v)
vr
#> Run Length Encoding
#>   lengths: int [1:7] 3 4 1 1 2 1 3
#>   values : chr [1:7] "A" "B" NA NA "C" "B" "C"

inverse.rle(vr)
#>  [1] "A" "A" "A" "B" "B" "B" "B" NA  NA  "C" "C" "B" "C" "C" "C"

w <- v
w[is.na(w)] <- "ZZZ"
w
#>  [1] "A"   "A"   "A"   "B"   "B"   "B"   "B"   "ZZZ" "ZZZ" "C"   "C"   "B"   "C"   "C"  
#> [15] "C"

wr <- rle(w)
wr
#> Run Length Encoding
#>   lengths: int [1:6] 3 4 2 2 1 3
#>   values : chr [1:6] "A" "B" "ZZZ" "C" "B" "C"

# Replace the ZZZ's with NA in the RLE-coded data
wr$values[ wr$values=="ZZZ" ] <- NA
wr
#> Run Length Encoding
#>   lengths: int [1:6] 3 4 2 2 1 3
#>   values : chr [1:6] "A" "B" NA "C" "B" "C"

w2 <- inverse.rle(wr)
w2
#>  [1] "A" "A" "A" "B" "B" "B" "B" NA  NA  "C" "C" "B" "C" "C" "C"

# Suppose this is the factor we have to work with
f <- factor(v)
f
#>  [1] A    A    A    B    B    B    B    <NA> <NA> C    C    B    C    C    C   
#> Levels: A B C

# Store the levels in the factor.
# This isn't strictly necessary, but it is useful for preserving order of levels
f_levels <- levels(f)
f_levels
#> [1] "A" "B" "C"

fc <- as.character(f)
fc[ is.na(fc) ] <- "ZZZ"
fc
#>  [1] "A"   "A"   "A"   "B"   "B"   "B"   "B"   "ZZZ" "ZZZ" "C"   "C"   "B"   "C"   "C"  
#> [15] "C"

fr <- rle(fc)
fr
#> Run Length Encoding
#>   lengths: int [1:6] 3 4 2 2 1 3
#>   values : chr [1:6] "A" "B" "ZZZ" "C" "B" "C"

# Replace the ZZZ's with NA in the RLE-coded data
fr$values[ fr$values=="ZZZ" ] <- NA
fr
#> Run Length Encoding
#>   lengths: int [1:6] 3 4 2 2 1 3
#>   values : chr [1:6] "A" "B" NA "C" "B" "C"

# Invert RLE coding and convert back to a factor
f2 <- inverse.rle(fr)
f2 <- factor(f, levels=f_levels)
f2
#>  [1] A    A    A    B    B    B    B    <NA> <NA> C    C    B    C    C    C   

# Sample data
x <- c(NA,NA, "A","A", "B","B","B", NA,NA, "C", NA,NA,NA, "A","A","B", NA,NA)

goodIdx <- !is.na(x)
goodIdx
#>  [1] FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE
#> [15]  TRUE  TRUE FALSE FALSE

# These are the non-NA values from x only
# Add a leading NA for later use when we index into this vector
goodVals <- c(NA, x[goodIdx])
goodVals
#>  [1] NA  "A" "A" "B" "B" "B" "C" "A" "A" "B"

# Fill the indices of the output vector with the indices pulled from
# these offsets of goodVals. Add 1 to avoid indexing to zero.
fillIdx <- cumsum(goodIdx)+1
fillIdx
#>  [1]  1  1  2  3  4  5  6  6  6  7  7  7  7  8  9 10 10 10

# The original vector with gaps filled
goodVals[fillIdx]
#>  [1] NA  NA  "A" "A" "B" "B" "B" "B" "B" "C" "C" "C" "C" "A" "A" "B" "B" "B"

fillNAgaps <- function(x, firstBack=FALSE) {
  ## NA's in a vector or factor are replaced with last non-NA values
  ## If firstBack is TRUE, it will fill in leading NA's with the first
  ## non-NA value. If FALSE, it will not change leading NA's.
  
  # If it's a factor, store the level labels and convert to integer
  lvls <- NULL
  if (is.factor(x)) {
    lvls <- levels(x)
    x    <- as.integer(x)
  }
  
  goodIdx <- !is.na(x)
  
  # These are the non-NA values from x only
  # Add a leading NA or take the first good value, depending on firstBack   
  if (firstBack)   goodVals <- c(x[goodIdx][1], x[goodIdx])
  else             goodVals <- c(NA,            x[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  
  x <- goodVals[fillIdx]
  
  # If it was originally a factor, convert it back
  if (!is.null(lvls)) {
    x <- factor(x, levels=seq_along(lvls), labels=lvls)
  }
  
  x
}



# Sample data
x <- c(NA,NA, "A","A", "B","B","B", NA,NA, "C", NA,NA,NA, "A","A","B", NA,NA)
x
#>  [1] NA  NA  "A" "A" "B" "B" "B" NA  NA  "C" NA  NA  NA  "A" "A" "B" NA  NA

fillNAgaps(x)
#>  [1] NA  NA  "A" "A" "B" "B" "B" "B" "B" "C" "C" "C" "C" "A" "A" "B" "B" "B"

# Fill the leading NA's with the first good value
fillNAgaps(x, firstBack=TRUE)
#>  [1] "A" "A" "A" "A" "B" "B" "B" "B" "B" "C" "C" "C" "C" "A" "A" "B" "B" "B"

# It also works on factors
y <- factor(x)
y
#>  [1] <NA> <NA> A    A    B    B    B    <NA> <NA> C    <NA> <NA> <NA> A    A    B    <NA>
#> [18] <NA>
#> Levels: A B C

fillNAgaps(y)
#>  [1] <NA> <NA> A    A    B    B    B    B    B    C    C    C    C    A    A    B    B   
#> [18] B   
#> Levels: A B C

