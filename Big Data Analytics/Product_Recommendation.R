######## Setting Environment ##########################################################
Sys.setenv("AWS_ACCESS_KEY_ID" = "XXXXXX",
           "AWS_SECRET_ACCESS_KEY" =
             "XXXXX",
           "AWS_DEFAULT_REGION" = "XXXXX")

library(ggplot2)
library(SparkR)

#Starting spark session
sc = sparkR.session(master='local')

#Setting s3 storage path
S3_BUCKET_NAME <- "s3a://sumansparkcasestudy/data"

####################### Exploratory Analysis of Kindle_Store data ######################

#Loading Kindle Data from s3 to R environment

ar_Kindle_Store <- SparkR::read.df(path=paste(S3_BUCKET_NAME, "/reviews_Kindle_Store_5.json", sep = ""), source="json")

#having a lookg of the data
showDF(ar_Kindle_Store, numRow = 10, truncate = TRUE)

#Checking missing values.
#-------------------------

#isNull() fuction is throwing error when used at spark data frame level. Hence, using isNull at column level. Here, we are applying 
#isNull() only for the columns which are useful for analysis.

#Checking missing value for columns helpful, overall, reviewText and reviewerID
count(where(ar_Kindle_Store,isNull(ar_Kindle_Store$helpful)))# 0 missing value
count(where(ar_Kindle_Store,isNull(ar_Kindle_Store$overall))) # 0 missing value
count(where(ar_Kindle_Store,isNull(ar_Kindle_Store$reviewText))) # 0 missing value
count(where(ar_Kindle_Store,isNull(ar_Kindle_Store$reviewerID))) # 0 missing value

## Creating a view on top of orignal Kindle data
createOrReplaceTempView(ar_Kindle_Store, "vw_Kindle_store")

## Checking average rating on total sold kindle quantity
showDF(SparkR::sql("select round(mean(overall),2) as AvgRating from vw_Kindle_store")) #4.35

### Calculating market size of Kindle by counting distinct reviewerID as a proxy method.
Kindle_Market_size<- as.numeric(take(SparkR::sql("select count(distinct reviewerID) from vw_Kindle_store"),1)) #68223

#### Calculating total quantity of Kindle sold by counting total no of ratings as a proxy method. We have taken ratings into consideration
#because there is a possibility that the customer who gives rating may or may not provide a review. 
Kindle_Sold_quantity<- as.numeric(take(SparkR::sql("select count(overall) from vw_Kindle_store"),1)) #982619

## Calculating no of happy customers for Kindle product category by counting reviews for which rating is 5 and review length is greater
#than thousand characters as a proxy method.
kindle_no_of_happy_customer<- as.numeric(take(SparkR::sql("select count(*)
                                                          from vw_Kindle_store
                                                          where overall = 5 and length(reviewText)> 1000"),1)) #81834

#-------------------------------------------------------------------
## Analysing helpullness score for Kindle category product line
#-------------------------------------------------------------------

# We are taking minimum threshold for calculating helpfulness score as 10 votes

Kindle_Helpful_Review<- SparkR::sql("select asin,
                                    helpful[0] as helpful_vote, 
                                    helpful[1] as total_vote,
                                    helpful[0]/helpful[1] as helpfulness_score,
                                    overall as rating,
                                    reviewText,
                                    length(reviewText) as reviewLength,
                                    reviewTime,
                                    reviewerID,
                                    reviewerName,
                                    summary,
                                    unixReviewTime
                                    from vw_Kindle_store where helpful[1]>10")

#Calculating total no of helpful review for kindle
nrow(Kindle_Helpful_Review) # 18218


# Visualizing distribution of helpfullness score based on review length
#----------------------------------------------------------------------

#checking minimum reviewLength
showDF(agg(Kindle_Helpful_Review,minReviewLength = min(Kindle_Helpful_Review$reviewLength)))#0

#checking maximum reviewLength
showDF(agg(Kindle_Helpful_Review,maxReviewLength = max(Kindle_Helpful_Review$reviewLength)))# 17830

# We are binning reviewLength of bin size 500 in increasing order.

Kindle_Helpful_Review$reviewLengthBin <- Kindle_Helpful_Review$reviewLength / 500

# Finding average Helpfulness score for each bin group of reviewe length
Kindle_Helpfulness_review <- collect(
  summarize(
    groupBy(Kindle_Helpful_Review, cast(Kindle_Helpful_Review$reviewLengthBin, "integer")), 
    avgHelpfulness_score = mean(Kindle_Helpful_Review$helpfulness_score) 
  )
)

base::colnames(Kindle_Helpfulness_review)[1]<- "reviewLength_Bin"

ggplot(Kindle_Helpfulness_review,aes(x = reviewLength_Bin, y = avgHelpfulness_score))+geom_line(stat = "identity")+
  scale_x_continuous(breaks = Kindle_Helpfulness_review$reviewLength_Bin[seq(1,length(Kindle_Helpfulness_review$reviewLength_Bin), by = 1)])+
  labs(x = "Review Length (500)", y = "Average Helpfulness Score") 

#From the line graph we can see that helpfulness score is above 0.8 upto review length 6000. After that trend is not explainable.   


#Visualizing distribution of helpfullness score based on rating.
#---------------------------------------------------------------

Kindle_Helpfulness_rating<- collect(
  summarize(
    groupBy(Kindle_Helpful_Review,Kindle_Helpful_Review$rating),
    avgHelpfulness_score =mean(Kindle_Helpful_Review$helpfulness_score)
  )
)

ggplot(Kindle_Helpfulness_rating,aes(x = rating, y = avgHelpfulness_score))+geom_line()+
  labs(x = "Customer Rating", y = "Average Helpfulness Score") 

#We can clearly see in the graph that average helpfulness score increases as customer rating increases for Kindle products. So, we should give a higher
#weightage to higher customer rating.

####################### Exploratory Analysis of CDs and Vinyl prodcut line ######################

#Loading CDs and Vinyl Data from s3 to R environment

ar_CDs_Vinyl <- SparkR::read.df(path=paste(S3_BUCKET_NAME, "/reviews_CDs_and_Vinyl_5.json", sep = ""), source="json")

#having a look of the data.
showDF(ar_CDs_Vinyl,numRow = 10, truncate = TRUE)

#Checking missing values.
#-------------------------

#isNull() fuction is throwing error when used at spark data frame level. Hence, using isNull at column level. Here, we are applying 
#isNull() only for the columns which are useful for analysis.

#Checking missing value for columns helpful, overall, reviewText and reviewerID
count(where(ar_CDs_Vinyl,isNull(ar_CDs_Vinyl$helpful)))# 0 missing value
count(where(ar_CDs_Vinyl,isNull(ar_CDs_Vinyl$overall))) # 0 missing value
count(where(ar_CDs_Vinyl,isNull(ar_CDs_Vinyl$reviewText))) # 0 missing value
count(where(ar_CDs_Vinyl,isNull(ar_CDs_Vinyl$reviewerID))) # 0 missing value

## Creating a view on top of orignal CDs and Vinyl data
createOrReplaceTempView(ar_CDs_Vinyl, "vw_CDs_Vinyl")

## Checking average rating on total sold CDs and Vinyl quantity
showDF(SparkR::sql("select round(mean(overall),2) as AvgRating from vw_CDs_Vinyl")) #4.29

### Calculating market size of CDs and Vinyl by counting distinct reviewerID as a proxy method. 
CDs_Vinyl_Market_size<- as.numeric(take(SparkR::sql("select count(distinct reviewerID) from vw_CDs_Vinyl"),1)) #75258

#### Calculating total quantity of CDs and Vinyl sold by counting total no of ratings as a proxy method. We have taken ratings into consideration
#because there is a possibility that the customer who gives rating may or may not provide a review.
CDs_Vinyl_Sold_quantity<- as.numeric(take(SparkR::sql("select count(overall) from vw_CDs_Vinyl"),1)) #1097592

## Calculating no of happy customers for CDs and Vinyl product category by counting reviews for which rating is 5 and review length is greater
#than thousand characters as a proxy method.
CDs_Vinyl_no_of_happy_customer<- as.numeric(take(SparkR::sql("select count(*)
                                                             from vw_CDs_Vinyl
                                                             where overall = 5 and length(reviewText)> 1000"),1))#208001

#-------------------------------------------------------------------------
## Analysing helpullness score for CDs and Vinyl category product line
#-------------------------------------------------------------------------

# We are taking minimum threshold for helpful review as 10 votes

CDs_Vinyl_Helpful_Review<- SparkR::sql("select asin,
                                       helpful[0] as helpful_vote, 
                                       helpful[1] as total_vote,
                                       helpful[0]/helpful[1] as helpfulness_score,
                                       overall as rating,
                                       reviewText,
                                       length(reviewText) as reviewLength,
                                       reviewTime,
                                       reviewerID,
                                       reviewerName,
                                       summary,
                                       unixReviewTime
                                       from vw_CDs_Vinyl where helpful[1]>10")

#Calculating total no of helpful review for CDs and Vinyl
nrow(CDs_Vinyl_Helpful_Review) # 146324

# Visualizing distribution of helpfullness score based on review length
#----------------------------------------------------------------------

#checking minimum reviewLength
showDF(agg(CDs_Vinyl_Helpful_Review,minReviewLength = min(CDs_Vinyl_Helpful_Review$reviewLength)))#0

#checking maximum reviewLength
showDF(agg(CDs_Vinyl_Helpful_Review,maxReviewLength = max(CDs_Vinyl_Helpful_Review$reviewLength)))# 32643

# We are binning reviewLength of bin size 1000 in increasing order.

CDs_Vinyl_Helpful_Review$reviewLengthBin <- CDs_Vinyl_Helpful_Review$reviewLength / 1000

# Finding average Helpfulness score for each bin group of reviewe length
CDs_Vinyl_Helpfulness_review <- collect(
  summarize(
    groupBy(CDs_Vinyl_Helpful_Review, cast(CDs_Vinyl_Helpful_Review$reviewLengthBin, "integer")), 
    avgHelpfulness_score = mean(CDs_Vinyl_Helpful_Review$helpfulness_score) 
  )
)

base::colnames(CDs_Vinyl_Helpfulness_review)[1]<- "reviewLength_Bin"

ggplot(CDs_Vinyl_Helpfulness_review,aes(x = reviewLength_Bin, y = avgHelpfulness_score))+geom_line(stat = "identity")+
  scale_x_continuous(breaks = CDs_Vinyl_Helpfulness_review$reviewLength_Bin[seq(1,length(CDs_Vinyl_Helpfulness_review$reviewLength_Bin), by = 1)])+
  labs(x = "Review Length (1000)", y = "Average Helpfulness Score")  


#From the line graph we can see that helpfulness score is above 0.8 upto review length 8000.
#After that trend is not explainable.   



#Visualizing distribution of helpfullness score based on ratings.
#----------------------------------------------------------------

CDs_Vinyl_Helpfulness_rating<- collect(
  summarize(
    groupBy(CDs_Vinyl_Helpful_Review,CDs_Vinyl_Helpful_Review$rating),
    avgHelpfulness_score =mean(CDs_Vinyl_Helpful_Review$helpfulness_score)
  )
)

ggplot(CDs_Vinyl_Helpfulness_rating,aes(x = rating, y = avgHelpfulness_score))+geom_line()+
  labs(x = "Customer Rating", y = "Average Helpfulness Score") 
#We can clearly see in the graph that average helpfulness score increases as customer rating increases for CDs and Vinyl products. So, we should give a higher
#weightage to higher customer rating product.


####################### Exploratory Analysis of Movies and TV product line ######################

#Loading Movies and TV Data from s3 to R environment

ar_Movies_TV <- SparkR::read.df(path=paste(S3_BUCKET_NAME, "/reviews_Movies_and_TV_5.json", sep = ""), source="json")

#Having a look of the data.
showDF(ar_Movies_TV,numRow = 10, truncate = TRUE)

#Checking missing values.
#-------------------------

#isNull() fuction is throwing error when used at spark data frame level. Hence, using isNull at column level. Here, we are applying 
#isNull() only for the columns which are useful for analysis.

#Checking missing value for columns helpful, overall, reviewText and reviewerID
count(where(ar_Movies_TV,isNull(ar_Movies_TV$helpful)))# 0 missing value
count(where(ar_Movies_TV,isNull(ar_Movies_TV$overall))) # 0 missing value
count(where(ar_Movies_TV,isNull(ar_Movies_TV$reviewText))) # 0 missing value
count(where(ar_Movies_TV,isNull(ar_Movies_TV$reviewerID))) # 0 missing value


## Creating a view on top of orignal Movies and TV data
createOrReplaceTempView(ar_Movies_TV, "vw_Movies_TV")

## Checking average rating on total sold Movies and TV quantity
showDF(SparkR::sql("select round(mean(overall),2) as AvgRating from vw_Movies_TV")) #4.11

### Calculating market size of Movies and TV by counting distinct reviewerID as a proxy method. 
Movies_TV_Market_size<- as.numeric(take(SparkR::sql("select count(distinct reviewerID) from vw_Movies_TV"),1)) #123960

## Calculating total quantity of Movies and TV sold by counting total no of ratings as a proxy method. We have taken ratings into consideration
#because there is a possibility that the customer who gives rating may or may not provide a review.
Movies_TV_Sold_quantity<- as.numeric(take(SparkR::sql("select count(overall) from vw_Movies_TV"),1)) #1697533

## Calculating no of happy customers for Movies and TV product category by counting reviews for which rating is 5 and review length is greater
#than thousand characters as a proxy method.
Movies_TV_no_of_happy_customer<- as.numeric(take(SparkR::sql("select count(*) 
                                                             from vw_Movies_TV
                                                             where overall = 5 and length(reviewText)> 1000"),1))#212963

#-------------------------------------------------------------------------
## Analysing helpullness score for Movies and TV category product line
#-------------------------------------------------------------------------

# We are taking minimum threshold for helpful review as 10 votes

Movies_TV_Helpful_Review<- SparkR::sql("select asin,
                                       helpful[0] as useful_vote, 
                                       helpful[1] as total_vote,
                                       helpful[0]/helpful[1] as helpfulness_score,
                                       overall as ratings,
                                       reviewText,
                                       length(reviewText) as reviewLength,
                                       reviewTime,
                                       reviewerID,
                                       reviewerName,
                                       summary,
                                       unixReviewTime
                                       from vw_Movies_TV where helpful[1]>10")

#Calculating total no of helpful review for CDs and Vinyl
nrow(Movies_TV_Helpful_Review) # 191616

# Visualizing distribution of helpfullness score based on review length
#----------------------------------------------------------------------

#checking minimum reviewLength
showDF(agg(Movies_TV_Helpful_Review,minReviewLength = min(Movies_TV_Helpful_Review$reviewLength)))#0

#checking maximum reviewLength
showDF(agg(Movies_TV_Helpful_Review,maxReviewLength = max(Movies_TV_Helpful_Review$reviewLength)))# 32572

# We are binning reviewLength of bin size 1000 in increasing order.

Movies_TV_Helpful_Review$reviewLengthBin <- Movies_TV_Helpful_Review$reviewLength / 1000

# Finding average Helpfulness score for each bin group of reviewe length
Movies_TV_Helpfulness_review <- collect(
  summarize(
    groupBy(Movies_TV_Helpful_Review, cast(Movies_TV_Helpful_Review$reviewLengthBin, "integer")), 
    avgHelpfulness_score = mean(Movies_TV_Helpful_Review$helpfulness_score) 
  )
)

base::colnames(Movies_TV_Helpfulness_review)[1]<- "reviewLength_Bin"

ggplot(Movies_TV_Helpfulness_review,aes(x = reviewLength_Bin, y = avgHelpfulness_score))+geom_line(stat = "identity")+
  scale_x_continuous(breaks = Movies_TV_Helpfulness_review$reviewLength_Bin[seq(1,length(Movies_TV_Helpfulness_review$reviewLength_Bin), by = 1)])+
  labs(x = "Review Length (1000)", y = "Average Helpfulness Score")

#From the line graph we can see that helpfulness score is above 0.77 from 2000 to 10500. After that trend is not explainable

#Visualizing distribution of helpfullness score based on ratings.
#----------------------------------------------------------------

Movies_TV_Helpfulness_rating<- collect(
  summarize(
    groupBy(Movies_TV_Helpful_Review,Movies_TV_Helpful_Review$ratings),
    avgHelpfulness_score =mean(Movies_TV_Helpful_Review$helpfulness_score)
  )
)

ggplot(Movies_TV_Helpfulness_rating,aes(x = ratings, y = avgHelpfulness_score))+geom_line()+
  labs(x = "Customer Rating", y = "Average Helpfulness Score")

#We can clearly see in the graph that average helpfulness score increases as customer rating increases for Movies and TV products. So, we should give a higher
#weightage to higher customer rating product.

########################################### Finalizing recommendation for the media company ####################################################

#1. Comparing market size among three product category
#-----------------------------------------------------

# Calculating minimum market size among three product categories
min_market_size <- min(Kindle_Market_size,CDs_Vinyl_Market_size,Movies_TV_Market_size)

#Calculating approximate ratio of market size of three product categories
market_size_ratio<- paste(round(Kindle_Market_size/min_market_size),":",round(CDs_Vinyl_Market_size/min_market_size),":",round(Movies_TV_Market_size/min_market_size))
market_size_ratio
#We can see that market size of Movies and TV category products is almost 2 times that of Kindle and CDs & Vinyl categories.

#2. Comparing no of products sold (purchased heavily) among three product categories:
#-----------------------------------------------------------------------------------

#Calculating minimum no of products sold among three product categories
min_product_sold<- min(Kindle_Sold_quantity,CDs_Vinyl_Sold_quantity,Movies_TV_Sold_quantity)

#Calculating approximate ratio of product sold of three product categories
product_sold_ratio<- paste(round(Kindle_Sold_quantity/min_product_sold),":",round(CDs_Vinyl_Sold_quantity/min_product_sold),":",round(Movies_TV_Sold_quantity/min_product_sold))
product_sold_ratio
#We can see that product sold of Movies and TV category products is almost 2 times that of Kindle and CDs & Vinyl categories.

#3. Comparing no of happy customers among three product categories:
#------------------------------------------------------------------

#Calculating minimum no of happy customers among three product categories
min_no_of_happy_customer<- min(kindle_no_of_happy_customer,CDs_Vinyl_no_of_happy_customer,Movies_TV_no_of_happy_customer)

#Calculating approximate ratio of no of happy customers of three product categories
no_of_happy_customer_ratio<- paste(round(kindle_no_of_happy_customer/min_no_of_happy_customer),":",
                                   round(CDs_Vinyl_no_of_happy_customer/min_no_of_happy_customer),":",
                                   round(Movies_TV_no_of_happy_customer/min_no_of_happy_customer))
no_of_happy_customer_ratio
#We can see that no.of happy customers for product categories CDs & Vinyl and Movies & TV are approximately 3 times that of product category kindle

############## Recommendation ##################                             
#By observing the above statistics for the three product categories, on the basis of market size, product demand and no of happy customers parameters,  
#we recommend the media company to invest in Movies and TV category product line.





