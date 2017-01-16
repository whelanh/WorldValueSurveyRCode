# analyze survey data for free (http://asdfree.com) with the r language
# consumer expenditure survey
# replication of the output of various macros stored in the "CE macros.sas" example program
# using 2011 public use microdata

# # # # # # # # # # # # # # # # #
# # block of code to run this # #
# # # # # # # # # # # # # # # # #
# library(downloader)
# setwd( "C:/My Directory/CES/" )
# source_url( "https://raw.githubusercontent.com/ajdamico/asdfree/master/Consumer%20Expenditure%20Survey/2011%20fmly%20intrvw%20-%20analysis%20examples.R" , prompt = FALSE , echo = TRUE )
# # # # # # # # # # # # # # #
# # end of auto-run block # #
# # # # # # # # # # # # # # #

# this r script will review the example analyses of both imputed and non-imputed variables
# described in the "CE macros program documentation.doc" document
# in the folder "Programs 2011\SAS\" inside the bls documentation file
# http://www.bls.gov/cex/pumd/documentation/documentation11.zip


# contact me directly for free help or for paid consulting work

# anthony joseph damico
# ajdamico@gmail.com



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
###################################################################################################################
# prior to running this replication script, all ces 2011 public use microdata files must be loaded as R data      #
# files (.rda) on the local machine. running the "2010-2011 ces - download.R" script will create these files.     #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# https://github.com/ajdamico/asdfree/blob/master/Consumer%20Expenditure%20Survey/download%20all%20microdata.R      #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# that script will save a number of .rda files in C:/My Directory/CES/2011/ (or the working directory was chosen) #
###################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# set your working directory.
# the CES 2011 R data files (.rda) should have been
# stored in a year-specific directory within this folder.
# so if the file "fmli111x.rda" exists in the directory "C:/My Directory/CES/2011/intrvw/" 
# then the working directory should be set to "C:/My Directory/CES/"
# use forward slashes instead of back slashes

# uncomment this line by removing the `#` at the front..
setwd( "~/Downloads/SCF/" )
# ..in order to set your current working directory



# turn off scientific notation in most output

options( scipen = 20 )


library(mitools)		# allows analysis of multiply-imputed survey data
library(stringr) 		# load stringr package (manipulates character strings easily)
library(plyr)			# contains the rbind.fill() function, which stacks two data frames even if they don't contain the same columns.  the rbind() function does not do this
library(survey)			# load survey package (analyzes complex design surveys)
library(downloader)		# downloads and then runs the source() function on scripts from github


# load two svyttest functions (one to conduct a df-adjusted t-test and one to conduct a multiply-imputed t-test)
source_url( "https://raw.githubusercontent.com/ajdamico/asdfree/master/Consumer%20Expenditure%20Survey/ces.svyttest.R" , prompt = FALSE )
# now that these two functions have been loaded into r, you can view their source code by uncommenting the two lines below
# svyttest.df
# svyttest.mi


# set this number to the year you would like to analyze..
year <- 2015


# r will now take the year you've selected and re-assign the current working directory
# to the year-specific folder based on what you'd set above
# so if you'd set C:/My Directory/CES/ above, it's now been changed to C:/My Directory/CES/2011/
setwd( paste( getwd() , year , sep = "/" ) )

# pull the last two digits of the year variable into a separate string
yr <- substr( year , 3 , 4 )


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# read in the five quarters of family data files (fmli)

# load all five R data files (.rda)
load( paste0( "./intrvw/fmli" , yr , "1x.rda" ) )
load( paste0( "./intrvw/fmli" , yr , "2.rda" ) )
load( paste0( "./intrvw/fmli" , yr , "3.rda" ) )
load( paste0( "./intrvw/fmli" , yr , "4.rda" ) )
load( paste0( "./intrvw/fmli" , as.numeric( yr ) + 1 , "1.rda" ) )

# save the first quarter's data frame into a new data frame called 'fmly'
fmly <- get( paste0( "fmli" , yr , "1x" ) )

# and create a new column called 'qtr' with all ones
fmly$qtr <- 1

# loop through the second, third, and fourth fmli data frames
for ( i in 2:4 ){

	# copy each quarter into a new data frame called 'x'
	x <- get( paste0( "fmli" , yr , i ) )

	# add a quarter variable (2, 3, then 4)
	x$qtr <- i
	
	# stack 'x' below what's already in the fmly data table
	# ..this stacks quarters 2, 3, and 4 below quarter 1
	fmly <- rbind.fill( fmly , x )
}

# repeat the steps above on the fifth quarter (which uses the following year's first quarter of data)
x <- get( paste0( "fmli" , as.numeric( yr ) + 1 , "1" ) )
x$qtr <- 5

# final stacking of the fifth quarter
fmly <- rbind.fill( fmly , x )
# now the 'fmly' data table contains everything needed for analyses

# delete the temporary data frame from memory
rm( x )

# also delete the data frames loaded by the five load() function calls above
rm( 
	list = 
		c( 
			paste0( "fmli" , yr , "1x" ) , 
			paste0( "fmli" , yr , 2:4 ) ,
			paste0( "fmli" , as.numeric( yr ) + 1 , "1" )
		)
)

# clear up RAM
gc()


# create a character vector containing 45 variable names (wtrep01, wtrep02, ... wtrep44 and finlwt21)
wtrep <- c( paste0( "wtrep" , str_pad( 1:44 , 2 , pad = "0" ) ) , "finlwt21" )

# immediately loop through each weight column (stored in the wtrep vector)
# and overwrite all missing values (NA) with zeroes
for ( i in wtrep ) fmly[ is.na( fmly[ , i ] ) , i ] <- 0

# create a new variable in the fmly data table called 'totalexp'
# that contains the sum of the total expenditure from the current and previous quarters
fmly$totalexp <- rowSums( fmly[ , c( "totexppq" , "totexpcq" ) ] , na.rm = TRUE )

# immediately convert missing values (NA) to zeroes
fmly[ is.na( fmly$totalexp ) , "totalexp" ] <- 0

# annualize the total expenditure by multiplying the total expenditure by four,
# creating a new variable 'annexp' in the fmly data table
fmly <- transform( fmly , annexp = totalexp * 4 )


# the "CE macros.sas" file creates estimates that match the mse = TRUE option set here.
# in order to match the sas software provided by the bureau of labor statistics, keep this set to TRUE

# if this option is set to TRUE
# R will exactly match SUDAAN results and Stata with the MSE option results
options( survey.replicates.mse = TRUE )
# otherwise if it is commented out or set to FALSE
# R will exactly match Stata without the MSE option results

# Stata svyset command notes can be found here: http://www.stata.com/help.cgi?svyset


# add a column called 'one' to the fmly data table containing 1s throughout
fmly$one <- 1


# create the survey design as a balanced repeated replication survey object, 
# with 44 replicate weights
fmly.design <- 
	svrepdesign( 
		repweights = "wtrep[0-9]+" , 
		weights = ~finlwt21 , 
		data = fmly 
	)

# after its creation, explore these attributes by typing the object into the console..
# print a basic description of the replicate design
fmly.design

# print the available attributes of this object
attributes( fmly.design )

# access one of the attributes.. hey how about the degrees of freedom?
fmly.design$degf


#####################
# analysis examples #
#####################

# count the total (unweighted) number of records in fmly #
unwtd.count( 
	~one , 
	fmly.design 
)

# broken out by the urban/rural variable #
svyby(
	~one ,
	~bls_urbn ,
	fmly.design ,
	unwtd.count
)


# calculate the mean of a linear variable #

# average annual household expenditure - nationwide
svymean(
	~annexp ,
	~age_ref%/%10,
	design = fmly.design
)

# by urban/rural
svyby(
	~annexp ,
	~bls_urbn ,
	design = fmly.design ,
	svymean
)


# calculate the distribution of a categorical variable #

# sex_ref should be treated as a factor (categorical) variable
# instead of a numeric (linear) variable
# this update statement converts it.
# the svyby command below will not run without this
fmly.design <-
	update(
		sex_ref = factor( sex_ref ) ,
		fmly.design
	)


# percent of households headed by males vs. females - nationwide
svymean(
	~sex_ref ,
	design = fmly.design
)


# by urban/rural
svyby(
	~sex_ref ,
	~bls_urbn ,
	design = fmly.design ,
	svymean
)


# calculate the median and other percentiles #

################### HUGH WHELAN CODE ##################################################
library(reldist)
# minimum, 25th, 50th, 75th, maximum
# annual expenditure in the united states
exp<-svyquantile(
	~annexp ,
	design = fmly.design ,
	c( seq(0,1,0.01) )
)

incbtx <- svyquantile(
  ~fincbtxm ,
  design = fmly.design ,
  c( seq(0,1,0.01) )
)

incatx <- svyquantile(
  ~finatxem ,
  design = fmly.design ,
  c( seq(0,1,0.01) )
)

gini(exp)
gini(incbtx)
gini(incatx)


########################### END HUGH WHELAN CODE #######################################


# by urban/rural
svyby(
	~annexp ,
	~bls_urbn ,
	design = fmly.design ,
	svyquantile ,
	c( 0 , .25 , .5 , .75 , 1 ) ,
	ci = T
)


######################
# subsetting example #
######################

# restrict the fmly.design object to
# households headed by females only
fmly.female <-
	subset(
		fmly.design ,
		sex_ref %in% 2
	)
# now any of the above commands can be re-run
# using fmly.female object
# instead of the fmly.design object
# in order to analyze households headed by females only

# calculate the mean of a linear variable #

# average household expenditure - nationwide, 
# restricted to households headed by females
svymean(
	~annexp ,
	design = fmly.female
)

# remove this subset design to clear up memory
rm( fmly.female )

# clear up RAM
gc()


######################################
# CE macros.sas replication examples #
######################################

# replicate the first macro shown in the "CE macros program documentation.doc" document

# the example macro (seen on page 7) looks like this, without the comments (#)
	# %MEAN_VARIANCE(DSN = FMLY, 
		# FORMAT = BLS_URBN $URBN.,
		# USE_WEIGHTS = YES,
		# BYVARS = BLS_URBN, 
		# ANALVARS = ANNEXP FINCBTXM, 
		# IMPUTED_VARS = FINCBTX1-FINCBTX5,
		# CL = 99, 
		# DF = RUBIN87,
		# TITLE1 = COMPUTING MEANS AND VARIANCES,
		# TITLE2 = VARIABLES FROM THE FAMILY FILE,
		# TITLE3 = ,
		# XOUTPUT = 
	# ); 

# instead of exporting all of these results into a large text output (like sas does)
# the following steps will produce each of the components, one at a time


# count the total (unweighted) number of records in fmly #
# broken out by the urban/rural variable, as specified in the sas macro call above
svyby( ~one , ~bls_urbn , fmly.design , unwtd.count )


# calculate means and standard errors, and save the results into a new object
# but also print the results to the screen.

# r hint: when assigning ( <- ) an object to another object, you can print the object to the screen
# at the same time as assigning ( <- ) it by encasing it in parentheses

# note that the following commands use svyby() outside of a svymean call
# as opposed to svymean() alone, because the results need to be broken out by
# the bls_urbn variable, as specified in the sas macro call above

# print and save the mean and standard error

#######################################################################################
# Code added by Hugh Whelan for Essay on "Retirement Crisis in America?"

# Data for Tables 3 and 4
fmly.design <-
  update(
    ageDecile = factor( age_ref%/%10 ) ,
    incDecile = factor(inclass),
    annexpPC = annexp/fam_size,
    fincbtxmPC = fincbtxm/fam_size, 
    fmly.design
  )


# Stats for 75+ year old age group
fmly.haveIncome <-
  subset(
    fmly.design ,
    age_ref>=75,
  )

# Household data for Table 3
svyby(~annexp, by = ~incDecile, denominator = ~fincbtxm , fmly.haveIncome, svyratio)
a<-svyby( ~annexp , ~incDecile , fmly.haveIncome , svymean , na.rm = TRUE )
b<-svyby( ~fincbtxm , ~incDecile , fmly.haveIncome , svymean , na.rm = TRUE )
e <- svyby( ~fam_size , ~incDecile , fmly.haveIncome , svymean , na.rm = TRUE )
# percent of households headed by inclass
c<-svymean(
  ~incDecile ,
  design = fmly.haveIncome
)
d<-merge(a,b,by="incDecile")
d$new <- coef(c)
# Per Capita Data for Table 4
svyby(~annexpPC, by = ~incDecile, denominator = ~fincbtxmPC , fmly.haveIncome, svyratio)
a<-svyby( ~annexpPC , ~incDecile , fmly.haveIncome , svymean , na.rm = TRUE )
b<-svyby( ~fincbtxmPC , ~incDecile , fmly.haveIncome , svymean , na.rm = TRUE )
e <- svyby( ~fam_size , ~incDecile , fmly.haveIncome , svymean , na.rm = TRUE )
# percent of households headed by inclass
c<-svymean(
  ~incDecile ,
  design = fmly.haveIncome
)
d<-merge(a,b,by="incDecile")
d$new <- coef(c)

# 65 to 74 year old Age Group Data
fmly.haveIncome <-
  subset(
    fmly.design ,
    age_ref>=65 & age_ref <75,
  )  

# Household data for Table 3
svyby(~annexp, by = ~incDecile, denominator = ~fincbtxm , fmly.haveIncome, svyratio)
a<-svyby( ~annexp , ~incDecile , fmly.haveIncome , svymean , na.rm = TRUE )
b<-svyby( ~fincbtxm , ~incDecile , fmly.haveIncome , svymean , na.rm = TRUE )
# percent of households headed by inclass
c<-svymean(
  ~incDecile ,
  design = fmly.haveIncome
)
d<-merge(a,b,by="incDecile")
d$new <- coef(c)

# Per Capita Data for Table 4
svyby(~annexpPC, by = ~incDecile, denominator = ~fincbtxmPC , fmly.haveIncome, svyratio)
a<-svyby( ~annexpPC , ~incDecile , fmly.haveIncome , svymean , na.rm = TRUE )
b<-svyby( ~fincbtxmPC , ~incDecile , fmly.haveIncome , svymean , na.rm = TRUE )
e <- svyby( ~fam_size , ~incDecile , fmly.haveIncome , svymean , na.rm = TRUE )
# percent of households headed by inclass
c<-svymean(
  ~incDecile ,
  design = fmly.haveIncome
)
d<-merge(a,b,by="incDecile")
d$new <- coef(c)

################################ END Hugh Whelan Code #################################
