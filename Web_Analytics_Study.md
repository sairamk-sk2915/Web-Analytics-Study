Web Analytics Study
================
Satya Shiva Sai Ram Kamma
2024-04-22

<center>

### <u>Revenue Optimization Study and Traffic Source Analysis</u>

</center>

#### Project Overview:

<p>
In this project we are going to do detail analysis of revenue
optimization strategies and traffic sources using a dataset called
(news_website_ddataser.csv). The aim is to use information we gather
from data to find ways to make more money and improve where our website
visitors are coming from. This helps us make better decisions to
increase our overall profits.
</p>

#### WHY?

<p>
Understanding what content people like and how to make money from it is
super important for digital news sites. By figuring out what articles or
topics are popular and how to make the most money from ads or
subscriptions, these sites can stay ahead of the competition and keep
readers coming back for more. It’s all about knowing what works and
making smart decisions to stay profitable and relevant in the online
news world.
</p>

#### WHAT?

<p>
Working on finding ways to make more profits and keep people interested
in visiting website. This means we’ll try different things like ads
section better spots, making articles content better for poor pages,
making the website easier to use(pgespeed), and using data to help us
make smart choices. By doing all this, we hope to make more profit and
keep our audience happy and coming back for more.
</p>

#### HOW?

<p>
To reach our goals, we’ll use a mix of strategies. This includes
conducting thorough market research and audience analysis to understand
trends and preferences, implementing targeted marketing campaigns to
attract new visitors, optimizing our website for user experience and
engagement, testing different revenue models to find the most effective
ones, and continuously monitoring and analyzing performance metrics to
refine our strategies over time.
</p>
<p>
Some other factor we can consider to get better results are:
</p>

<b>Which time of day across different traffic sources drives the highest
number of sessions and conversions?</b>

<p>

1.  Identifying when each traffic source reaches its peak traffic allows
    us to optimize your website and its resources, such as content
    scheduling, ad campaign scheduling, and improving the responsiveness
    and loading speed of the website.

2.  Understanding how the time of day, the source of traffic, and
    website performance relate to one another allows us to make
    decisions based on data that improve user experience, maximize
    resource allocation

    </p>

<b>Is there a significant correlation between content category and
session duration?</b>

<p>

1.  We can learn what type of content is the most valuable and engaging
    to the audience by studying how long users spend on various content
    categories.

2.  This information can direct the work we do in creating content,
    ensuring that we focus on topics that interest users and hold their
    attention for a longer period of time.

3.  Identifying parts on a website where users quickly lose interest and
    quit can be improved by studying the relationship between content
    categories and session time. This enables to enhance the user
    experience, layout, or quality of the content in specific
    categories.

    </p>

<p>
Are there any specific landing pages that are particularly successful
for different traffic sources or campaigns?
<p>

#### Find and collect data

<p>
The variables included in this dataset
</p>

    - Time of Day
    - Traffic Source
    - Landing Page
    - Campaign  
    - Device Category   
    - Avg Session Duration
    - Content Category  
    - Total Sessions    
    - Conversion Rate   
    - Total revenue

#### Dependent Variables

    - Total Sessions    
    - Conversion Rate   
    - Total revenue

#### Independent Variables

    - Time of Day
    - Traffic Source
    - Landing Page
    - Campaign  
    - Device Category   
    - Avg Session Duration
    - Content Category  

[View the CSV file](News_Website_Dataset.csv)

#### Data Dictionary

<p>

<b>Total Sessions:</b> Total number of unique sessions on the website
for a specific timeframe (e.g., Day, Week, Month).

<b>Conversion Rate:</b> Percentage of visitors who complete a desired
action (e.g., Polls, Newsletter subscription).

<b>Total Revenue:</b> The total amount generated throughout the
sessions.

<b>Time of Day:</b> Categorical variable with 4 levels (Morning,
Afternoon, Evening, Night).

<b>Traffic Source:</b> Categorical variable indicating where visitors
originated from (e.g., Organic Search, Search, Referral, Direct,
Social).

<b>Landing Page:</b> The first page a visitor viewed on your website.

<b>Campaign:</b> Categorical variable indicating which marketing
campaign a visitor originated from.

<b>Device Category:</b> Categorical variable indicating the device used
to access the website (e.g., Desktop, Mobile, Tablet).

<b>Average Session Duration:</b> The average time spent by visitors on
your website per session (continuous).

<b>Content Category:</b> Categorical variable classifying the content
type of the visited page (e.g., Article, Category, About Us, Contact Us,
Home Page(/)).
</p>

#### Data Collection

``` r
#libraries
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 4.3.2

``` r
library(ggplot2)
library(FactoMineR)
```

    ## Warning: package 'FactoMineR' was built under R version 4.3.3

``` r
library(openxlsx)
```

    ## Warning: package 'openxlsx' was built under R version 4.3.3

``` r
library(cluster)
library(factoextra)
```

    ## Warning: package 'factoextra' was built under R version 4.3.2

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(magrittr)
library(NbClust)
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.3.3

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ROCR)
```

    ## Warning: package 'ROCR' was built under R version 4.3.3

``` r
library(pROC)
```

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
News_Website_Dataset <- read_excel("News Website Dataset_2.xlsx")
```

``` r
orignal_file <- "News Website Dataset_2.xlsx"
content_convert_to_numeric <- function(category) {
  category_map <- c("Blog" = 1, "Product" = 2, "Homepage" = 3, "About Us" = 4)
  return(category_map[category])
}

News_Website_Dataset$Content_Category_Num <- sapply(News_Website_Dataset$Content_Category, content_convert_to_numeric)


Device_Category_convert_to_numeric <- function(Device_Category) {
  category_map2 <- c("Desktop" = 1, "Mobile" = 2, "Tablet" = 3)
  return(category_map2[Device_Category])
}

News_Website_Dataset$Device_Category_Num <- sapply(News_Website_Dataset$Device_Category, Device_Category_convert_to_numeric)

# Append the new column to the Excel file
write.xlsx(News_Website_Dataset, orignal_file , sheet = "Sheet 1", append = TRUE)
```

``` r
head(News_Website_Dataset)   # View the first few rows of the dataset
```

    ## # A tibble: 6 × 12
    ##   Time_of_Day Traffic_Source Landing_Page      Campaign     Content_Category
    ##   <chr>       <chr>          <chr>             <chr>        <chr>           
    ## 1 Morning     Organic Search /blog/new-product SEO Campaign Blog            
    ## 2 Morning     Paid Search    /article-page     Paid         Product         
    ## 3 Morning     Referral       /catgory-page     Social Media Blog            
    ## 4 Morning     Direct         /                 -            Homepage        
    ## 5 Morning     Organic Social Mobile            -            About Us        
    ## 6 Afternoon   Organic Search /blog/new-product SEO Campaign Blog            
    ## # ℹ 7 more variables: Device_Category <chr>, Avg_Session_Duration <dbl>,
    ## #   Total_Sessions <dbl>, Conversion_Rate <dbl>, Total_revenue <dbl>,
    ## #   Content_Category_Num <dbl>, Device_Category_Num <dbl>

``` r
summary(News_Website_Dataset)   # Summary statistics for each variable
```

    ##  Time_of_Day        Traffic_Source     Landing_Page         Campaign        
    ##  Length:160         Length:160         Length:160         Length:160        
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  Content_Category   Device_Category    Avg_Session_Duration Total_Sessions 
    ##  Length:160         Length:160         Min.   :1.010        Min.   :100.9  
    ##  Class :character   Class :character   1st Qu.:2.092        1st Qu.:217.8  
    ##  Mode  :character   Mode  :character   Median :3.190        Median :317.2  
    ##                                        Mean   :3.085        Mean   :314.7  
    ##                                        3rd Qu.:4.172        3rd Qu.:417.5  
    ##                                        Max.   :4.981        Max.   :499.2  
    ##  Conversion_Rate  Total_revenue     Content_Category_Num Device_Category_Num
    ##  Min.   :0.1083   Min.   :  2.233   Min.   :1.0          Min.   :1.0        
    ##  1st Qu.:0.3143   1st Qu.: 83.385   1st Qu.:1.0          1st Qu.:1.0        
    ##  Median :0.5432   Median :164.559   Median :2.0          Median :2.0        
    ##  Mean   :0.5167   Mean   :167.906   Mean   :2.2          Mean   :1.8        
    ##  3rd Qu.:0.7346   3rd Qu.:245.305   3rd Qu.:3.0          3rd Qu.:2.0        
    ##  Max.   :0.8931   Max.   :346.407   Max.   :4.0          Max.   :3.0

``` r
str(News_Website_Dataset)   # Structure of the dataset
```

    ## tibble [160 × 12] (S3: tbl_df/tbl/data.frame)
    ##  $ Time_of_Day         : chr [1:160] "Morning" "Morning" "Morning" "Morning" ...
    ##  $ Traffic_Source      : chr [1:160] "Organic Search" "Paid Search" "Referral" "Direct" ...
    ##  $ Landing_Page        : chr [1:160] "/blog/new-product" "/article-page" "/catgory-page" "/" ...
    ##  $ Campaign            : chr [1:160] "SEO Campaign" "Paid" "Social Media" "-" ...
    ##  $ Content_Category    : chr [1:160] "Blog" "Product" "Blog" "Homepage" ...
    ##  $ Device_Category     : chr [1:160] "Desktop" "Mobile" "Tablet" "Desktop" ...
    ##  $ Avg_Session_Duration: num [1:160] 4.23 2.43 4.61 1.59 3.19 ...
    ##  $ Total_Sessions      : num [1:160] 325 487 316 417 219 ...
    ##  $ Conversion_Rate     : num [1:160] 0.355 0.152 0.401 0.248 0.504 ...
    ##  $ Total_revenue       : num [1:160] 244.2 83.7 127.5 167.1 247.1 ...
    ##  $ Content_Category_Num: Named num [1:160] 1 2 1 3 4 1 2 1 3 4 ...
    ##   ..- attr(*, "names")= chr [1:160] "Blog.Blog" "Product.Product" "Blog.Blog" "Homepage.Homepage" ...
    ##  $ Device_Category_Num : Named num [1:160] 1 2 3 1 2 1 2 3 1 2 ...
    ##   ..- attr(*, "names")= chr [1:160] "Desktop.Desktop" "Mobile.Mobile" "Tablet.Tablet" "Desktop.Desktop" ...

<p>
Data Cleaning
</p>

``` r
is.na(News_Website_Dataset)
```

    ##                   Time_of_Day Traffic_Source Landing_Page Campaign
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Product.Product         FALSE          FALSE        FALSE    FALSE
    ## Blog.Blog               FALSE          FALSE        FALSE    FALSE
    ## Homepage.Homepage       FALSE          FALSE        FALSE    FALSE
    ## About Us.About Us       FALSE          FALSE        FALSE    FALSE
    ##                   Content_Category Device_Category Avg_Session_Duration
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Product.Product              FALSE           FALSE                FALSE
    ## Blog.Blog                    FALSE           FALSE                FALSE
    ## Homepage.Homepage            FALSE           FALSE                FALSE
    ## About Us.About Us            FALSE           FALSE                FALSE
    ##                   Total_Sessions Conversion_Rate Total_revenue
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Product.Product            FALSE           FALSE         FALSE
    ## Blog.Blog                  FALSE           FALSE         FALSE
    ## Homepage.Homepage          FALSE           FALSE         FALSE
    ## About Us.About Us          FALSE           FALSE         FALSE
    ##                   Content_Category_Num Device_Category_Num
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Product.Product                  FALSE               FALSE
    ## Blog.Blog                        FALSE               FALSE
    ## Homepage.Homepage                FALSE               FALSE
    ## About Us.About Us                FALSE               FALSE

``` r
sum(is.na(News_Website_Dataset))
```

    ## [1] 0

#### Correltion and Coefficient

``` r
correlation_coefficient <- cor(News_Website_Dataset$Total_revenue, News_Website_Dataset$Conversion_Rate)
print(correlation_coefficient)
```

    ## [1] 0.2185369

``` r
plot(News_Website_Dataset$Conversion_Rate, News_Website_Dataset$Total_revenue,
     xlab = "Conversion Rate", ylab = "Total Revenue",
     main = "Scatter Plot of Total Revenue vs. Conversion Rate")

abline(lm(News_Website_Dataset$Total_revenue ~ News_Website_Dataset$Conversion_Rate), col = "red")
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
print(paste("Correlation Coefficient between Total Revenue and Avg Session Duration:", correlation_coefficient))
```

    ## [1] "Correlation Coefficient between Total Revenue and Avg Session Duration: 0.218536904909115"

``` r
# correlation  and coefficient B/W Total_revenue and Total Sessions
correlation_coefficient2 <- cor(News_Website_Dataset$Total_revenue, News_Website_Dataset$Total_Sessions)
print(correlation_coefficient2)
```

    ## [1] 0.06006431

``` r
plot(News_Website_Dataset$Total_Sessions, News_Website_Dataset$Total_revenue,
     xlab = "Total Sessions", ylab = "Total Revenue",
     main = "Scatter Plot of Total Revenue vs. Total Sessions")

abline(lm(News_Website_Dataset$Total_revenue ~ News_Website_Dataset$Total_Sessions), col = "blue")
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

##### Univariate Analysis

<p>
<b>Question :</b> What is the distribution of total revenue?
</p>
<p>
<b>Visualization:</b> Histogram of Total Revenue
</p>

``` r
hist(News_Website_Dataset$Total_revenue, 
     main = "Distribution of Total Revenue",
     xlab = "Total Revenue",
     ylab = "Frequency",
     col = "lightgreen",
     border = "black")
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
<p>
The histogram shows the distribution of total income. It means that most
of the income falls in the lower ranges, and is distributed to the
right. Small amount increases are considered excessive.
</p>

##### Bivariate Analysis:

<p>
<b>Question :</b> Is there a relationship between total revenue and
average session duration?
</p>
<p>
<b>Visualization:</b> Scatter plot of Total Revenue and Avg Session
Duration
</p>

``` r
ggplot(News_Website_Dataset, aes(x = Avg_Session_Duration, y = Total_revenue)) +
  geom_point(color = "blue") +
  labs(title = "Total Revenue and Avg Session Duration",
       x = "Average Session Duration",
       y = "Total Revenue")
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
<p>
The scatter plot suggests a positive correlation between total revenue
and average session duration, as higher revenue tends to coincide with
longer session durations.
</p>

##### Bivariate Analysis:

<p>
<b>Question :</b> How does total revenue vary across different traffic
sources?
</p>
<p>
<b>Visualization:</b> Box plot of Total Revenue by Traffic Source
</p>

``` r
ggplot(News_Website_Dataset, aes(x = Traffic_Source, y = Total_revenue, fill = Traffic_Source)) +
  geom_boxplot() +
  labs(title = "Total Revenue by Traffic Source",
       x = "Traffic Source",
       y = "Total Revenue")
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
<p>
The box plot shows variations in total revenue across different traffic
sources, with some sources having higher median revenues compared to
others.
</p>

##### Multivariate Analysis:

<p>
<b>Question :</b>How does total revenue vary across different device
categories and time of day?
</p>
<p>
<b>Visualization:</b> Line plot of Total Revenue by Time of Day, color
by Device Category
</p>

``` r
ggplot(News_Website_Dataset, aes(x = Time_of_Day, y = Total_revenue, color = Device_Category)) +  geom_line(size = 1.5) +
  labs(title = "Total Revenue by Time of Day (Colored by Device Category)",
       x = "Time of Day",
       y = "Total Revenue")
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
<p>
The line plot illustrates how total revenue varies across different
times of the day, with each line representing a different device
category. It helps identify revenue trends based on the time of day and
device usage.
</p>

##### 1. Total Revenue

<p>
Analyzing the univariate mean and variance of the “Total Revenue”
variable
</p>

``` r
mean_revenue <- mean(News_Website_Dataset$Total_revenue)
variance_revenue <- var(News_Website_Dataset$Total_revenue)

print(paste("Mean for Total Revenue:", mean_revenue))
```

    ## [1] "Mean for Total Revenue: 167.905995738624"

``` r
print(paste("Variance for Total Revenue:", variance_revenue))
```

    ## [1] "Variance for Total Revenue: 9585.83717298643"

``` r
#Box Plot for Total Revenue
ggplot(News_Website_Dataset, aes(x = "", y = Total_revenue)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Box Plot of Total Revenue", x = "", y = "Total Revenue")
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Q-Q plot for Total Revenue
qqnorm(News_Website_Dataset$Total_revenue, main = "Q-Q Plot of Total Revenue")
qqline(News_Website_Dataset$Total_revenue)
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->
<p>
The mean total revenue provides the average amount of revenue generated
across all observations in the dataset. It gives a central measure of
the revenue distribution. The variance of total revenue indicates the
spread or dispersion of revenue values around the mean. A higher
variance suggests that revenue values are more spread out from the mean,
while a lower variance suggests that revenue values are closer to the
mean. This analysis helps us understand the typical revenue amount and
the variability in revenue generation.
</p>

##### 2. Total Revenue by Device Category

<p>
Analyzing the mean and variance of Total Revenue across different Device
Categories
</p>

``` r
mean_revenue_device <- aggregate(Total_revenue ~ Device_Category, data = News_Website_Dataset, mean)
variance_revenue_device <- aggregate(Total_revenue ~ Device_Category, data = News_Website_Dataset, var)

print("Mean Total Revenue by Device Category")
```

    ## [1] "Mean Total Revenue by Device Category"

``` r
print(mean_revenue_device)
```

    ##   Device_Category Total_revenue
    ## 1         Desktop      166.2946
    ## 2          Mobile      170.6054
    ## 3          Tablet      165.7300

``` r
print("Variance of Total Revenue by Device Category")
```

    ## [1] "Variance of Total Revenue by Device Category"

``` r
print(variance_revenue_device)
```

    ##   Device_Category Total_revenue
    ## 1         Desktop      9304.748
    ## 2          Mobile     10512.562
    ## 3          Tablet      8866.886

``` r
# Bar plot for Mean Total Revenue by Device Category
ggplot(mean_revenue_device, aes(x = Device_Category, y = Total_revenue, fill = Device_Category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Total Revenue by Device Category", x = "Device Category", y = "Mean Total Revenue") +
  theme_minimal()
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# Violin plot for Distribution of Total Revenue by Device Category
ggplot(News_Website_Dataset, aes(x = Device_Category, y = Total_revenue, fill = Device_Category)) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribution of Total Revenue by Device Category", x = "Device Category", y = "Total Revenue") +
  theme_minimal()
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
# Box plot for Total Revenue by Device Category
ggplot(News_Website_Dataset, aes(x = Device_Category, y = Total_revenue, fill = Device_Category)) +
  geom_boxplot() +
  labs(title = "Total Revenue by Device Category", x = "Device Category", y = "Total Revenue") +
  theme_minimal()
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->
<p>
This analysis calculates the mean and variance of total revenue for each
device category separately. It helps us understand how revenue varies
across different device categories. The mean total revenue by device
category provides insight into the average revenue generated by users
using each type of device. The variance of total revenue by device
category shows the variability in revenue generation among users of the
same device category. This analysis can help identify which device
categories contribute the most to revenue and how consistent revenue
generation is across different devices
</p>

#### PCA

<p>
1 Decide how many Principal Components (PCs) you want to keep and why
</p>
<p>
2 Explain the variate representation each PCs
</p>
<p>
3 Perform some visualization using PCs.
</p>

``` r
News_Website_Dataset_num <- read_excel("News Website Dataset_2.xlsx", range = cell_cols("G:L"))
cor(News_Website_Dataset_num[-1])
```

    ##                      Total_Sessions Conversion_Rate Total_revenue
    ## Total_Sessions           1.00000000     -0.01673150   0.060064307
    ## Conversion_Rate         -0.01673150      1.00000000   0.218536905
    ## Total_revenue            0.06006431      0.21853690   1.000000000
    ## Content_Category_Num    -0.02967319     -0.06129320   0.016897530
    ## Device_Category_Num      0.00135264     -0.05827177   0.002866678
    ##                      Content_Category_Num Device_Category_Num
    ## Total_Sessions                -0.02967319         0.001352640
    ## Conversion_Rate               -0.06129320        -0.058271765
    ## Total_revenue                  0.01689753         0.002866678
    ## Content_Category_Num           1.00000000        -0.183339699
    ## Device_Category_Num           -0.18333970         1.000000000

``` r
News_Website_Dataset_num_pca <- prcomp(News_Website_Dataset_num[,-1],scale=TRUE)
News_Website_Dataset_num_pca
```

    ## Standard deviations (1, .., p=5):
    ## [1] 1.1088180 1.0889941 1.0031862 0.9290720 0.8456105
    ## 
    ## Rotation (n x k) = (5 x 5):
    ##                              PC1         PC2         PC3        PC4        PC5
    ## Total_Sessions        0.13656253  0.12199967 -0.95414315 -0.1451764 -0.1870867
    ## Conversion_Rate       0.69527119  0.02858051  0.26658303 -0.3201658 -0.5849858
    ## Total_revenue         0.69245889  0.01914609 -0.07549623  0.4960431  0.5180499
    ## Content_Category_Num -0.05218542 -0.71014143 -0.09509569  0.5416585 -0.4365074
    ## Device_Category_Num  -0.12540340  0.69255455  0.06165599  0.5804869 -0.4048157

``` r
summary(News_Website_Dataset_num_pca)
```

    ## Importance of components:
    ##                           PC1    PC2    PC3    PC4    PC5
    ## Standard deviation     1.1088 1.0890 1.0032 0.9291 0.8456
    ## Proportion of Variance 0.2459 0.2372 0.2013 0.1726 0.1430
    ## Cumulative Proportion  0.2459 0.4831 0.6844 0.8570 1.0000

``` r
(eigen_News_Website_Dataset <- News_Website_Dataset_num_pca$sdev^2)
```

    ## [1] 1.2294774 1.1859082 1.0063825 0.8631748 0.7150572

``` r
names(eigen_News_Website_Dataset) <- paste("PC",1:4,sep="")
eigen_News_Website_Dataset
```

    ##       PC1       PC2       PC3       PC4      <NA> 
    ## 1.2294774 1.1859082 1.0063825 0.8631748 0.7150572

``` r
sumlambdas <- sum(eigen_News_Website_Dataset)
sumlambdas
```

    ## [1] 5

``` r
propvar <- eigen_News_Website_Dataset/sumlambdas
propvar
```

    ##       PC1       PC2       PC3       PC4      <NA> 
    ## 0.2458955 0.2371816 0.2012765 0.1726350 0.1430114

``` r
cumvar_News_Website_Dataset <- cumsum(propvar)
cumvar_News_Website_Dataset
```

    ##       PC1       PC2       PC3       PC4      <NA> 
    ## 0.2458955 0.4830771 0.6843536 0.8569886 1.0000000

``` r
matlambdas <- rbind(eigen_News_Website_Dataset,propvar,cumvar_News_Website_Dataset)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)
```

    ##                        PC1    PC2    PC3    PC4   <NA>
    ## Eigenvalues         1.2295 1.1859 1.0064 0.8632 0.7151
    ## Prop. variance      0.2459 0.2372 0.2013 0.1726 0.1430
    ## Cum. prop. variance 0.2459 0.4831 0.6844 0.8570 1.0000

``` r
summary(News_Website_Dataset_num_pca)
```

    ## Importance of components:
    ##                           PC1    PC2    PC3    PC4    PC5
    ## Standard deviation     1.1088 1.0890 1.0032 0.9291 0.8456
    ## Proportion of Variance 0.2459 0.2372 0.2013 0.1726 0.1430
    ## Cumulative Proportion  0.2459 0.4831 0.6844 0.8570 1.0000

``` r
News_Website_Dataset_num_pca$rotation
```

    ##                              PC1         PC2         PC3        PC4        PC5
    ## Total_Sessions        0.13656253  0.12199967 -0.95414315 -0.1451764 -0.1870867
    ## Conversion_Rate       0.69527119  0.02858051  0.26658303 -0.3201658 -0.5849858
    ## Total_revenue         0.69245889  0.01914609 -0.07549623  0.4960431  0.5180499
    ## Content_Category_Num -0.05218542 -0.71014143 -0.09509569  0.5416585 -0.4365074
    ## Device_Category_Num  -0.12540340  0.69255455  0.06165599  0.5804869 -0.4048157

``` r
print(News_Website_Dataset_num_pca)
```

    ## Standard deviations (1, .., p=5):
    ## [1] 1.1088180 1.0889941 1.0031862 0.9290720 0.8456105
    ## 
    ## Rotation (n x k) = (5 x 5):
    ##                              PC1         PC2         PC3        PC4        PC5
    ## Total_Sessions        0.13656253  0.12199967 -0.95414315 -0.1451764 -0.1870867
    ## Conversion_Rate       0.69527119  0.02858051  0.26658303 -0.3201658 -0.5849858
    ## Total_revenue         0.69245889  0.01914609 -0.07549623  0.4960431  0.5180499
    ## Content_Category_Num -0.05218542 -0.71014143 -0.09509569  0.5416585 -0.4365074
    ## Device_Category_Num  -0.12540340  0.69255455  0.06165599  0.5804869 -0.4048157

``` r
News_Website_Dataset_num_pca$x
```

    ##                 PC1          PC2          PC3          PC4         PC5
    ##   [1,]  0.253602469 -0.003529212 -0.300226891 -0.577271950  1.67444760
    ##   [2,] -1.511602830  0.424281593 -1.726990023 -0.075584104  0.16456547
    ##   [3,] -0.776629169  1.815233781  0.080336272  0.324623752 -0.12503235
    ##   [4,] -0.594356902 -1.150419598 -1.272420401 -0.007961944  0.64333198
    ##   [5,]  0.297286951 -0.994273618  0.577611018  1.525435809 -0.17584703
    ##   [6,]  0.956580623 -0.062375711  1.033892223 -1.425215583  0.34328873
    ##   [7,] -0.916936425  0.168293821  0.930954645 -0.160265016 -0.17419767
    ##   [8,]  0.761374590  1.983146013 -1.058817410  1.121296614  0.65468046
    ##   [9,] -0.459695822 -1.296493695 -0.254311661  0.698012484  1.51911368
    ##  [10,] -0.433158395 -1.093481052  1.135639236  1.450897798 -0.14118627
    ##  [11,]  0.477765250 -0.133464077  1.245110780 -1.072995428  0.90723570
    ##  [12,]  0.569002481  0.390364993 -0.426018969  0.066436666 -0.13618926
    ##  [13,]  0.847851243  1.925354185  0.081415889  0.019413157 -0.87736282
    ##  [14,] -1.863073332 -1.222037152 -1.034891225 -0.475341377  0.27980793
    ##  [15,] -0.405272866 -0.829305891 -1.049193623  1.100335760 -0.53333001
    ##  [16,]  0.317652262 -0.008716843  0.147035116 -1.282025491  0.70326836
    ##  [17,] -0.729865397  0.083645109  1.993749964 -0.550254175 -0.77432039
    ##  [18,]  0.977662402  2.028705000 -0.538406960 -0.451013890 -1.50973755
    ##  [19,] -0.985080323 -1.350954112  0.883018010 -1.058773989 -0.75723125
    ##  [20,]  0.640159543 -1.093406128  1.524569708  1.761453592  0.03100969
    ##  [21,] -1.479948132 -0.121706170  0.306458227 -1.405530669  0.91400671
    ##  [22,]  1.027123806  0.433018422 -0.224716106 -0.605526847 -1.13803935
    ##  [23,]  0.239103879  1.923796682 -0.357446714  0.287973272 -0.37318904
    ##  [24,] -0.633588070 -1.440162230  1.027191838  0.553363381  1.31495619
    ##  [25,] -1.760050360 -0.827618149 -1.556094923  0.708168776 -0.75031452
    ##  [26,]  0.496341132 -0.008936732  0.208968771 -1.220906003  0.74461351
    ##  [27,]  0.503803093  0.452139417 -1.010370323  0.054626964 -0.11624378
    ##  [28,]  1.463773073  2.006908410 -0.819563259  0.943847467  0.25973070
    ##  [29,]  0.981570706 -1.168073567 -0.548869203  0.462744399  0.91011291
    ##  [30,]  1.177732432 -0.819842839 -0.528532424  1.498221573 -0.35767277
    ##  [31,] -0.230288937  0.153928194 -1.801286046 -0.898170700  1.40494881
    ##  [32,]  1.067377621  0.257691622  0.785274976  0.528713016  0.32981888
    ##  [33,] -0.179602653  2.005791481 -1.810919407  1.244664949  1.04727167
    ##  [34,]  1.232838364 -1.096944542 -0.434418471 -0.738799136 -0.75156378
    ##  [35,] -1.173091866 -0.969664976 -0.438515364  1.599457512  0.27312689
    ##  [36,] -0.700771821  0.062326390 -1.171301799 -0.986013179  1.36397166
    ##  [37,]  0.679773510  0.472779166 -1.220627424  0.274323700  0.14756598
    ##  [38,] -0.100337727  1.820411590  0.773968202 -0.428447861 -1.29990029
    ##  [39,] -0.935812118 -1.116547996 -1.653273104 -0.199758651  0.47402099
    ##  [40,]  2.117445825 -0.718389967 -0.784903233  1.161760049 -0.99672882
    ##  [41,] -0.336883082 -0.135517612  0.963060712 -1.299398099  0.78847456
    ##  [42,]  0.263495723  0.409007244 -0.333552670 -0.734160258 -1.14359231
    ##  [43,]  0.201736790  1.737308815  1.524054788 -0.132284045 -0.99671992
    ##  [44,]  1.548701906 -1.121173231 -0.579958381  0.270340509  0.53403086
    ##  [45,]  0.098632201 -0.982397994  0.061559136  2.113831128  0.67114233
    ##  [46,]  0.263058525  0.154751672 -1.157045353 -1.661522973  0.25703914
    ##  [47,] -1.717427322  0.343624645 -1.187407440  0.081459754  0.39771393
    ##  [48,]  0.750084174  1.749456494  1.363881183  0.505106673 -0.25621108
    ##  [49,] -0.004737868 -1.229418438  0.201911838 -0.888039624 -0.71191042
    ##  [50,]  0.373958586 -0.818454134 -0.321051435  0.284433697 -1.81521949
    ##  [51,]  0.543588794 -0.133974055  1.399634911 -1.295594966  0.59031008
    ##  [52,] -0.805800849  0.114299959  1.553278505 -0.310510072 -0.42165786
    ##  [53,]  0.394189072  1.988865866 -0.857570552  0.274761974 -0.40502789
    ##  [54,]  0.454468135 -1.020280040 -1.054941184 -1.641375666 -1.76804653
    ##  [55,] -1.085079942 -1.129096253  1.441176205  0.838452637 -0.83184748
    ##  [56,]  0.333157382  0.064772925 -1.138870961 -0.071742429  2.36382881
    ##  [57,]  0.230460733  0.155172933  1.535714249  0.037597411 -0.17625007
    ##  [58,] -0.631411899  1.957143406 -0.745824898 -0.401612402 -1.09445857
    ##  [59,]  0.012222533 -1.232292643  0.081840512 -0.591423044 -0.31503478
    ##  [60,]  0.720611017 -0.826363686 -0.272728281  0.670085440 -1.37577011
    ##  [61,] -0.545759365  0.026845808 -0.303916931 -1.882945912  0.10120056
    ##  [62,]  0.355806107  0.312579745  0.238479721 -0.072934038 -0.30186347
    ##  [63,] -1.208542408  1.964035963 -1.559598417  0.470841496  0.22323158
    ##  [64,]  1.709207835 -1.019612842 -0.899226044 -0.715151658 -0.80371851
    ##  [65,] -1.221366945 -1.132337297  1.077986904  1.456550319  0.03583397
    ##  [66,]  0.250204569  0.074476290 -0.275882372 -1.975859489 -0.19285054
    ##  [67,] -1.834788442  0.052627571  1.588469981 -0.325666144 -0.22478081
    ##  [68,] -2.371925707  1.654917172  0.743441514  0.252925714  0.09307249
    ##  [69,]  1.437201078 -1.295106976  1.184366724 -0.215332989 -0.15670795
    ##  [70,]  0.033403430 -0.980047381 -0.052277967  2.227185435  0.84057694
    ##  [71,]  1.714847966 -0.142202855  1.915903441 -0.989129406  0.73148917
    ##  [72,]  0.711045976  0.309086664  0.561198476 -0.281278835 -0.66748990
    ##  [73,] -1.927403173  1.809685486 -0.285375060 -0.012145465 -0.31665025
    ##  [74,] -0.240529409 -1.424991265  1.966165049 -1.121943345 -1.04015128
    ##  [75,]  2.194683723 -0.673242938 -1.204128010  1.261149974 -0.86474847
    ##  [76,] -1.294526823  0.010048203 -0.636540656 -1.692572682  0.52674150
    ##  [77,]  0.535391302  0.231768431  0.876045867  0.278662477  0.10553207
    ##  [78,] -0.564475227  1.805735932  0.383185339  0.117201508 -0.45831005
    ##  [79,] -1.673670123 -1.325590545  0.039882833 -0.559174659  0.08745856
    ##  [80,] -0.855296925 -0.847504534 -1.358795309  1.565146135  0.19430614
    ##  [81,] -1.101857255 -0.163210254  0.765357839 -1.195326837  1.09719318
    ##  [82,] -1.641528233  0.057636721  1.631506472 -0.304357173 -0.23896836
    ##  [83,] -0.753979108  1.933939162 -1.071894493  0.503957894  0.15269588
    ##  [84,]  1.400277265 -1.275327821  0.592057123  0.541992638  0.88432552
    ##  [85,]  2.195425768 -0.657322022 -1.361423423  1.288226010 -0.82287231
    ##  [86,]  0.816347454  0.094566401 -0.651456507 -1.045250821  0.94338752
    ##  [87,] -0.437774693  0.398643633 -1.315048779  0.635880923  0.87100395
    ##  [88,] -0.770546196  1.959719901 -1.196649869  0.281131991 -0.13658959
    ##  [89,]  0.048171842 -1.116177946 -0.563837213 -1.325424134 -1.27838756
    ##  [90,] -1.005509155 -0.841984704 -0.887345391  0.417789536 -1.32316926
    ##  [91,]  2.379916702  0.235049752 -1.107512961 -1.058915698  0.61030898
    ##  [92,]  0.292719136  0.395797276 -1.100476876  0.993452014  1.18526377
    ##  [93,] -0.258321702  1.633586901  1.583605329  1.093384990  0.73497255
    ##  [94,] -1.636601770 -1.356865650  0.179413907 -0.249491469  0.48779321
    ##  [95,]  0.405683762 -0.741228094 -1.173214987  0.614480166 -1.34944836
    ##  [96,]  1.612821116  0.195048842 -1.339627185 -0.678170977  1.29012373
    ##  [97,]  0.954267611  0.354951174 -0.159863918  0.547974645  0.41491829
    ##  [98,]  0.861547990  1.893293364  0.432582685 -0.086123129 -1.03430431
    ##  [99,]  0.943796547 -1.303079581  1.267660738 -0.723363339 -0.73330676
    ## [100,] -1.565769181 -0.941250943 -0.593579516  1.018631414 -0.41311438
    ## [101,] -0.745904284  0.108427401 -1.218152648 -1.737651251  0.37187212
    ## [102,]  1.172497426  0.289475016  0.785833997  0.081744465 -0.28921732
    ## [103,] -1.665796331  1.757164577  0.279533661  0.070490244 -0.28300785
    ## [104,]  0.924671487 -1.336529107  1.104345124  0.149198765  0.44173221
    ## [105,] -2.614367832 -1.157735412  0.875925480  0.910505629 -0.38987238
    ## [106,] -1.040535297 -0.212356165  1.355595320 -1.416945656  0.76622441
    ## [107,]  0.081844110  0.491185757 -0.954887934 -1.142548939 -1.62697399
    ## [108,] -1.105850703  1.809633764  0.181140097 -0.096223862 -0.62074975
    ## [109,]  0.700281548 -1.034253149 -1.576081967 -0.158407417  0.17903514
    ## [110,]  2.186260613 -0.678298602 -1.094989087  1.130998298 -1.04079726
    ## [111,]  0.263377505 -0.047302481  0.445058499 -1.239840605  0.75995164
    ## [112,]  0.988396647  0.312617300  0.232630449  0.560933277  0.41028292
    ## [113,]  0.877063676  1.683919496  1.594291212  1.321457594  0.79803553
    ## [114,]  1.539101765 -1.364999398  1.563080704  0.367257224  0.58524195
    ## [115,] -0.743631537 -0.796228767 -1.056100089  0.208348452 -1.65204586
    ## [116,] -0.475259776 -0.094826714  0.395266411 -1.050562726  1.17129403
    ## [117,]  1.785578690  0.397122061  0.170752863  0.004126333 -0.49995711
    ## [118,]  1.134067707  1.992777242 -0.500978786  0.252453712 -0.60521306
    ## [119,]  0.157742882 -1.184189802 -0.057999736 -1.011524636 -0.90152481
    ## [120,]  0.731028532 -0.849114998 -0.312893240  1.150685135 -0.73487968
    ## [121,]  0.225530822  0.122930805 -0.752623901 -1.923786392 -0.10025199
    ## [122,]  1.991345080  0.369262739  0.372120498  0.302417353 -0.15295563
    ## [123,]  0.112431102  1.668407676  1.247373505  1.501376171  1.21352231
    ## [124,] -1.775182349 -1.162966149 -1.495246962 -0.527311735  0.20900104
    ## [125,] -1.546230608 -0.895525950 -1.083191244  1.186132107 -0.17532392
    ## [126,] -0.671293867 -0.078321452  0.339527003 -1.422191501  0.71887045
    ## [127,]  0.325375088  0.112597569  2.172215255 -0.353216475 -0.74195587
    ## [128,] -0.219736442  1.772786586  1.101507146 -0.350748554 -1.18308599
    ## [129,] -0.614418798 -1.409956549  1.065713583 -0.023804932  0.53892807
    ## [130,] -1.716730984 -0.813277852 -1.437641691  0.275822844 -1.34112353
    ## [131,]  1.606075083 -0.039293086  0.742996563 -0.626570200  1.28249363
    ## [132,]  1.046048847  0.367280921 -0.196806274  0.496081394  0.32754092
    ## [133,] -0.021827169  1.814813053  0.009045589  1.212303310  0.90245929
    ## [134,] -0.709095383 -1.221354978 -0.657024660 -0.074595037  0.55569994
    ## [135,]  0.325349712 -0.931840836  0.561633009  0.504380370 -1.54431055
    ## [136,]  0.171878105  0.113372576 -1.307865319 -0.746829570  1.50315101
    ## [137,] -1.186947320  0.179696776  0.556664279  0.094190522  0.23682473
    ## [138,]  0.120229920  1.660723708  1.482254306  1.191558354  0.78947022
    ## [139,]  1.270013684 -1.177536211  0.161010595 -0.450528088 -0.39690041
    ## [140,]  1.683546246 -0.691759633 -1.140874597  0.956358208 -1.16551032
    ## [141,]  2.370945484  0.101624426 -0.060858730 -0.767868095  0.96158620
    ## [142,]  0.116923631  0.395643647 -0.676625992  0.010233701 -0.10587553
    ## [143,] -0.782143540  1.918229204 -0.734171638  0.100276942 -0.39287821
    ## [144,]  1.777863706 -1.352520817  1.530180718  0.449862826  0.64606847
    ## [145,] -2.464035573 -1.088844925  0.382591940  0.813529086 -0.53278270
    ## [146,]  0.404261444  0.175724591 -1.334510847 -1.544715253  0.38964543
    ## [147,] -1.439521060  0.271949906 -0.216707342 -0.266746201 -0.16246341
    ## [148,] -0.566642682  1.638586782  1.597527103  0.676460602  0.24330783
    ## [149,] -0.104455737 -1.281486398  0.055834089  0.193938945  0.75916121
    ## [150,] -1.845496052 -1.058279667  0.452045652  0.761406883 -0.73626511
    ## [151,]  0.731073337  0.151126516 -0.532062328 -2.336004484 -0.76613019
    ## [152,] -0.169749608  0.135319659  1.879718634 -0.673080217 -1.05292275
    ## [153,] -2.283116755  1.706847280  0.349565359  0.197848994  0.01543555
    ## [154,]  0.254880122 -1.361796626  0.798469962  0.512531020  1.08040696
    ## [155,] -0.545680509 -1.029821832  0.628609898  1.211624345 -0.41775254
    ## [156,]  0.558155754 -0.013492819 -0.272565003 -0.157976187  2.16852148
    ## [157,] -0.259032921  0.151106879  0.506875357  1.598204672  2.04956829
    ## [158,]  0.744341689  1.780184006  1.268894320  0.150350428 -0.72503494
    ## [159,] -0.586960656 -1.225238312 -0.386939537 -0.404959545  0.07862991
    ## [160,] -1.244815723 -0.955253457 -0.307907485  1.029221931 -0.47779842

``` r
encoded_data <- model.matrix(~News_Website_Dataset$Device_Category - 1, data = News_Website_Dataset_num)
numerical_data <- cbind(News_Website_Dataset_num[, -which(names(News_Website_Dataset_num) == "Device_Category")], encoded_data)

#PCA
pca_result <- prcomp(numerical_data, scale = TRUE)

# Scree plot
plot(pca_result$sdev^2, type = "b", xlab = "Principal Component", ylab = "Variance Explained")
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
loadings <- pca_result$rotation
print(loadings)
```

    ##                                                       PC1        PC2       PC3
    ## News_Website_Dataset$Device_CategoryDesktop -7.071068e-01  0.3535534 0.6123724
    ## News_Website_Dataset$Device_CategoryMobile   7.071068e-01  0.3535534 0.6123724
    ## News_Website_Dataset$Device_CategoryTablet   1.221245e-15 -0.8660254 0.5000000

``` r
# Data projection onto first two PCs
data_projection <- as.data.frame(pca_result$x[, 1:2])


# Plot
plot(data_projection$PC1, data_projection$PC2, 
     xlab = "PC1", ylab = "PC2", 
     main = "Data Visualization using PCs - 1")
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

#### Clustering

<p>
For each model, decide the optimal number of clusters and explain why
</p>
<p>
Show the membership for each cluster
</p>
<p>
show a visualization of the cluster and membership using the first two
Principal Components
</p>

``` r
# Load necessary libraries
library(cluster)
library(factoextra)
library(magrittr)
library(NbClust)

data <- read_excel("News Website Dataset.xlsx")

data_num <- data[, c("Avg_Session_Duration", "Total_Sessions", "Total_revenue")]
dist_matrix <- dist(data_num)

# Hierarchical clustering
hclust_model <- hclust(dist_matrix)

plot(hclust_model)
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
num_clusters <- 3
clusters <- cutree(hclust_model, k = num_clusters)

# Membership for each cluster
table(clusters)
```

    ## clusters
    ##  1  2  3 
    ##  4 12  4

``` r
# Visualize cluster and membership using first two Principal Components
pca_result <- prcomp(data_num, scale = TRUE)
fviz_cluster(list(data = pca_result$x[, 1:2], cluster = clusters))
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
# Non-hierarchical clustering (k-means)
num_clusters <- 2  
kmeans_model <- kmeans(data_num, centers = num_clusters)

# Membership for each cluster
table(kmeans_model$cluster)
```

    ## 
    ##  1  2 
    ## 12  8

``` r
# Visualize cluster and membership using first two Principal Components
fviz_cluster(list(data = pca_result$x[, 1:2], cluster = kmeans_model$cluster))
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

``` r
# Read the dataset
data <- read_excel("News Website Dataset_2.xlsx")

# Select numerical variables for clustering
data_num <- data[, c("Avg_Session_Duration", "Total_Sessions", "Total_revenue")]

# Perform hierarchical clustering
dist_matrix <- dist(data_num)
hclust_model <- hclust(dist_matrix)

# Decide on the optimal number of clusters based on the dendrogram
num_clusters_hclust <- 3  # Replace with chosen number of clusters

# Perform non-hierarchical clustering (k-means)
num_clusters_kmeans <- 2  # Replace with the chosen number of clusters
kmeans_model <- kmeans(data_num, centers = num_clusters_kmeans)

# Visualize cluster centers for k-means
fviz_cluster(kmeans_model, data = data_num, geom = "point", frame.type = "convex", 
             pointsize = 2, fill = "white", main = "K-means Cluster Centers")
```

    ## Warning: argument frame is deprecated; please use ellipse instead.

    ## Warning: argument frame.type is deprecated; please use ellipse.type instead.

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
# Visualize cluster and membership using first two Principal Components for k-means
pca_result <- prcomp(data_num, scale = TRUE)
fviz_cluster(kmeans_model, data = pca_result$x[, 1:2], geom = "point", 
             pointsize = 2, fill = "white", main = "K-means Clustering Result (PCA)")
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

``` r
# Calculate silhouette information for k-means clustering
sil <- silhouette(kmeans_model$cluster, dist(data_num))

# Visualize the silhouette plot for k-means clustering
fviz_silhouette(sil, main = "Silhouette Plot for K-means Clustering")
```

    ##   cluster size ave.sil.width
    ## 1       1   77          0.36
    ## 2       2   83          0.41

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->

``` r
# Create a data frame with cluster membership
data_clustered <- cbind(data_num, Cluster = kmeans_model$cluster)

# Scatter plot of data points colored by cluster membership
plot(data_clustered$Avg_Session_Duration, data_clustered$Total_Sessions, 
     col = data_clustered$Cluster, pch = 16, 
     xlab = "Avg_Session_Duration", ylab = "Total_Sessions",
     main = "Scatter Plot of Clustering")
legend("topright", legend = unique(data_clustered$Cluster), 
       col = 1:num_clusters_kmeans, pch = 16, title = "Cluster")
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-16-4.png)<!-- -->

#### Factor Analysis

<p>
<b>1. Decide how many Factors are ideal for your dataset</b>
</p>
<p>
Parallel analysis suggests that the number of factors = 1 and the number
of components = 1
</p>
<p>
<b>2. Explain the output for your factor model</b>
</p>
<p>
<b>Standardized Loadings</b>
</p>
<p>
Avg_Session_Duration has low loadings on all factors, with the highest
loading on MR3 (0.32).
</p>
<p>
Total_Sessions has the highest loading on MR2 (0.34).
</p>
<p>
Total_revenue and Conversion_Rate both have the highest loadings on MR1
(0.48 and 0.50, respectively).
</p>
<p>
<b>SS Loadings</b>
</p>
<p>
MR1 explains 12% of the total variance., MR2 explains 4% of the total
variance., MR3 explains 3% of the total variance.
</p>
<p>
MR1 might be related to conversion rate and potentially session duration
(positive loadings).
</p>
<p>
MR2 is positively associated with the total number of sessions.
</p>
<p>
MR3 has weaker and mixed relationships with the variables.
</p>
<p>
we can conclude that a 3-factor solution adequately explains the
structure of the data. Each factor captures a unique aspect of the
underlying structure, with Total_revenue and Conversion_Rate loading
primarily on MR1, Total_Sessions on MR2, and Avg_Session_Duration on
MR3.
</p>
<p>
<b> 3&4 Show the columns that go into each factor and Perform some
visualizations using the factors</b>
</p>

``` r
data <- read_excel("News Website Dataset_2.xlsx")
data_num <- data[, c("Avg_Session_Duration", "Total_Sessions", "Total_revenue","Conversion_Rate")]
factor_model <- fa(data_num, nfactors = 3, rotate = "varimax")

fa.parallel(data_num[-1])
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  1

``` r
print(factor_model)
```

    ## Factor Analysis using method =  minres
    ## Call: fa(r = data_num, nfactors = 3, rotate = "varimax")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                        MR1   MR2   MR3   h2   u2 com
    ## Avg_Session_Duration -0.02  0.03  0.32 0.10 0.90 1.0
    ## Total_Sessions        0.01  0.34  0.03 0.12 0.88 1.0
    ## Total_revenue         0.48  0.18 -0.18 0.30 0.70 1.6
    ## Conversion_Rate       0.50 -0.07  0.05 0.25 0.75 1.1
    ## 
    ##                        MR1  MR2  MR3
    ## SS loadings           0.48 0.15 0.14
    ## Proportion Var        0.12 0.04 0.03
    ## Cumulative Var        0.12 0.16 0.19
    ## Proportion Explained  0.62 0.20 0.18
    ## Cumulative Proportion 0.62 0.82 1.00
    ## 
    ## Mean item complexity =  1.2
    ## Test of the hypothesis that 3 factors are sufficient.
    ## 
    ## df null model =  6  with the objective function =  0.06 with Chi Square =  9.18
    ## df of  the model are -3  and the objective function was  0 
    ## 
    ## The root mean square of the residuals (RMSR) is  0 
    ## The df corrected root mean square of the residuals is  NA 
    ## 
    ## The harmonic n.obs is  160 with the empirical chi square  0  with prob <  NA 
    ## The total n.obs was  160  with Likelihood Chi Square =  0  with prob <  NA 
    ## 
    ## Tucker Lewis Index of factoring reliability =  2.959
    ## Fit based upon off diagonal values = 1
    ## Measures of factor score adequacy             
    ##                                                     MR1   MR2   MR3
    ## Correlation of (regression) scores with factors    0.63  0.39  0.37
    ## Multiple R square of scores with factors           0.39  0.15  0.14
    ## Minimum correlation of possible factor scores     -0.21 -0.69 -0.72

``` r
factor_loadings <- factor_model$loadings
print(factor_loadings)
```

    ## 
    ## Loadings:
    ##                      MR1    MR2    MR3   
    ## Avg_Session_Duration                0.320
    ## Total_Sessions               0.342       
    ## Total_revenue         0.483  0.176 -0.184
    ## Conversion_Rate       0.496              
    ## 
    ##                 MR1   MR2   MR3
    ## SS loadings    0.48 0.154 0.140
    ## Proportion Var 0.12 0.039 0.035
    ## Cumulative Var 0.12 0.159 0.193

``` r
#some visualizations
fa.plot(factor_model)      # See Correlations within Factors
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

``` r
fa.diagram(factor_model)   # Visualize the relationship
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-17-3.png)<!-- -->

#### <b>Multiple Regression Analysis</b>

<p>
Multiple regression analysis is a statistical technique that extends
simple linear regression by considering the combined effect of two or
more independent variables on a single dependent variable. It helps us
understand how these independent variables influence the dependent
variable, predict the value of the dependent variable based on the
independent ones, and assess the relative importance of each independent
variable while accounting for the influence of others. This makes it a
powerful tool for various fields like business, finance, and social
science to uncover relationships and make predictions.
</p>

#### Model Development

<p>
We will load the data and convert the data into numerical. we will split
the data into training and testing data. then we will perform multiple
regression model
</p>

#### Model Acceptance

<p>
Model acceptance involves evaluating the performance of the multiple
regression model on unseen data or a testing dataset
</p>
<p>
It evaluates the model’s performance through coefficient summaries,
diagnostic plots, and confidence intervals. Overall, it aims to assess
the model’s acceptance by analyzing its fit to the data and the
significance of predictor variables.
</p>

#### Residual Analysis

<p>
Residual analysis is crucial for evaluating the assumptions of the
multiple regression model and identifying any patterns or trends in the
residuals
</p>
<p>
It will generate diagnostic plots, including a plot of residuals
vs. fitted values, a QQ plot of residuals, and a scale-location plot.
These plots can help you assess the assumptions of the multiple
regression model.
</p>

``` r
mydata <- read_excel("News Website Dataset_2.xlsx")
data_num <- mydata[, c("Avg_Session_Duration", "Total_Sessions", "Total_revenue","Conversion_Rate")]
```

#### Model Development

``` r
model <- lm(Total_revenue ~ Total_Sessions
+ Conversion_Rate + Avg_Session_Duration, data = mydata)
model_fit <- summary(model)
print(model_fit)
```

    ## 
    ## Call:
    ## lm(formula = Total_revenue ~ Total_Sessions + Conversion_Rate + 
    ##     Avg_Session_Duration, data = mydata)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -184.85  -81.61  -10.46   78.97  212.17 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          119.54318   33.79714   3.537 0.000533 ***
    ## Total_Sessions         0.05439    0.06500   0.837 0.404024    
    ## Conversion_Rate       92.69082   32.81633   2.825 0.005354 ** 
    ## Avg_Session_Duration  -5.39689    6.35919  -0.849 0.397362    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 96.03 on 156 degrees of freedom
    ## Multiple R-squared:  0.05618,    Adjusted R-squared:  0.03803 
    ## F-statistic: 3.095 on 3 and 156 DF,  p-value: 0.02866

<p>
In this step, we loaded the dataset and fitted a multiple regression
model using the lm() function. The model predicts the Total Revenue
based on sTotal Sessions, Conversion Rate and Avg session duration
</p>

#### Model Acceptance

``` r
coefficients(model_fit)
```

    ##                          Estimate  Std. Error    t value     Pr(>|t|)
    ## (Intercept)          119.54317823 33.79714046  3.5370797 0.0005330632
    ## Total_Sessions         0.05438749  0.06500008  0.8367297 0.4040244587
    ## Conversion_Rate       92.69081501 32.81633125  2.8245331 0.0053537621
    ## Avg_Session_Duration  -5.39689133  6.35918737 -0.8486763 0.3973618488

``` r
confint(model_fit,level=0.95)
```

    ##      2.5 % 97.5 %

``` r
fitted(model_fit)
```

    ## NULL

#### Residual Analysis

``` r
library(GGally)
```

    ## Warning: package 'GGally' was built under R version 4.3.2

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
ggpairs(data=mydata, title="News Website Data")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
plot(model)
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-21-2.png)<!-- -->![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-21-3.png)<!-- -->![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-21-4.png)<!-- -->![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-21-5.png)<!-- -->

``` r
residuals(model_fit)
```

    ##            1            2            3            4            5            6 
    ##   96.9063072  -63.3452085  -21.5983314   10.5036248   86.1327874   -5.7575205 
    ##            7            8            9           10           11           12 
    ##  -70.0707498  145.3104876  106.9838636   45.0963398   14.5727938   29.8310979 
    ##           13           14           15           16           17           18 
    ##  -14.4660514 -102.7935785    2.4551071  -15.6960588 -123.0888193  -66.5997753 
    ##           19           20           21           22           23           24 
    ## -159.4312118  130.7408517 -110.2393455  -51.9239117   -0.3323999   78.8408473 
    ##           25           26           27           28           29           30 
    ##  -87.0582883    7.3599269   18.9900952  149.2526579  141.2738785  123.7961466 
    ##           31           32           33           34           35           36 
    ##   50.1271139   99.1466470  141.4334344  -18.0822273   50.5839276    7.0728911 
    ##           37           38           39           40           41           42 
    ##   79.3701884 -113.0420897  -26.2602478  107.0903384  -45.1905240 -107.8048420 
    ##           43           44           45           46           47           48 
    ##  -75.4089353  141.9198405  157.3065896  -49.2985774  -52.1220267   50.8102797 
    ##           49           50           51           52           53           54 
    ## -109.3173672  -75.3781492  -23.8833817  -91.3632496   12.7576593 -184.8522599 
    ##           55           56           57           58           59           60 
    ##  -80.1635624  165.3611312   -2.6866932 -117.5246959  -45.6684424   -4.3019543 
    ##           61           62           63           64           65           66 
    ## -115.7181760  -16.0395742   -9.0705558   -2.9343184   19.7157158 -118.0151976 
    ##           67           68           69           70           71           72 
    ## -139.3479658  -97.7078351   32.2761908  181.9362249   62.6935134  -20.2748190 
    ##           73           74           75           76           77           78 
    ## -117.3101609 -164.6807474  143.3974823 -114.9808970   49.9085057  -57.0011345 
    ##           79           80           81           82           83           84 
    ## -116.6948692   73.4158922  -61.0243793 -127.9659815   11.9916089  157.3621178 
    ##           85           86           87           88           89           90 
    ##  144.3488696   50.2371210   86.5083269  -19.5028011 -141.2681667 -111.0866966 
    ##           91           92           93           94           95           96 
    ##  103.5163212  165.5064321   75.5167803  -77.5501907  -30.1503203  147.6307296 
    ##           97           98           99          100          101          102 
    ##  114.1303249  -38.3763032  -48.5221613  -44.3464178  -94.6115941   37.2095682 
    ##          103          104          105          106          107          108 
    ## -113.0381888   67.6150879 -115.1379618  -85.9407806 -151.1830975  -95.6802143 
    ##          109          110          111          112          113          114 
    ##   39.5906614  125.4828213  -14.8145645  114.4673218  167.6948464  135.7407511 
    ##          115          116          117          118          119          120 
    ## -123.0112138  -16.1909357   69.4532910   32.6080973 -100.2953600   54.7270022 
    ##          121          122          123          124          125          126 
    ##  -90.6087276  104.8198065  152.3854425  -95.0608137  -23.6371842  -62.0294269 
    ##          127          128          129          130          131          132 
    ##  -74.0525364 -124.7143456   -1.6420125 -156.8580098  123.4698586  102.6680959 
    ##          133          134          135          136          137          138 
    ##  124.5092327  -11.8530874  -56.3698736   69.4740498  -38.6534685  110.5109199 
    ##          139          140          141          142          143          144 
    ##   11.0234754   80.4643643  139.4212543    8.7829812  -46.0762062  140.4859423 
    ##          145          146          147          148          149          150 
    ## -120.5971262  -34.2782902 -104.2335622   16.7411277   49.0735323 -109.1190913 
    ##          151          152          153          154          155          156 
    ## -134.5198168 -124.4338788 -111.2549407   98.8913694   11.3851605  152.3546055 
    ##          157          158          159          160 
    ##  212.1671614  -13.0161173  -37.4890318  -39.6152355

<p>
The residual vs. fitted plot is a tool used to evaluate the assumptions
and adequacy of a regression model. It helps to identify whether the
model adequately captures the underlying relationships in the data or if
there are issues that need to be addressed. The plot shows a pattern of
points around zero, the model is not appropriate.
</p>

#### Prediction

<p>
The predict() function will generate predicted values of the dependent
variable (Total_revenue) based on the provided predictors.
</p>

``` r
new_data <- data.frame(Time_of_Day = "Morning",
                       Traffic_Source = "Organic Search",
                       Landing_Page = "/blog/new-product",
                       Campaign = "SEO Campaign",
                       Content_Category = "Blog",
                       Device_Category = "Desktop",
                       Avg_Session_Duration = 4.23,
                       Total_Sessions = 325,
                       Conversion_Rate = 0.35)

# Make predictions
predicted_total_revenue <- predict(model, newdata = new_data)
predicted_total_revenue
```

    ##       1 
    ## 146.832

##### Model Accuracy

<p>
Model accuracy can be assessed using various metrics, such as R-squared,
adjusted R-squared, and root mean squared error (RMSE). Here’s how you
can calculate these metrics
</p>

``` r
#Model Accuracy
rsquared <- summary(model)$r.squared
cat("R-squared:", rsquared, "\n")
```

    ## R-squared: 0.05617748

``` r
adjusted_rsquared <- summary(model)$adj.r.squared
cat("Adjusted R-squared:", adjusted_rsquared, "\n")
```

    ## Adjusted R-squared: 0.03802704

``` r
predictions <- predict(model)
rmse <- sqrt(mean((data$Total_revenue - predictions)^2))
cat("RMSE:", rmse, "\n")
```

    ## RMSE: 94.81974

#### <b>Logistic Regression Analysis</b>

<p>
To perform logistic regression analysis, we will use the glm() function.
</p>

- Load all necessary packages
- Load Data. we Used read_excel() function to read data from excel
- Now we will use glm() function to fit a logistic regression model to
  the data.
- Now use summary() function for logistic regression model to view
  coefficients, standard errors, z-values, and p-values.
- For Residual Analysis use plot() function to get Plot diagnostic
  plots, including residuals vs. fitted values, QQ plot of residuals,
  and scale-location plot, to check for homoscedasticity and normality
  of residuals.

#### Model Development

``` r
mydata <- read_excel("News Website Dataset_2.xlsx")
```

``` r
threshold <- 200

mydata$Revenue_Binary <- ifelse(mydata$Total_revenue > threshold, 1, 0)

logit_model <- glm(Revenue_Binary ~  Total_Sessions
+ Conversion_Rate + Avg_Session_Duration, 
                    data = mydata, 
                    family = binomial)
```

<p>
The code reads a dataset and preprocesses it to create a binary outcome
variable based on a threshold.
</p>
<p>
It fits a logistic regression model using three predictor variables:
<p>
Total_Sessions, Conversion_Rate, and Avg_Session_Duration.
</p>
This model development process involves specifying the model formula,
fitting the model to the data, and obtaining a summary of the model’s
coefficients and statistical significance.
</p>

#### Model Acceptance

``` r
summary(logit_model)
```

    ## 
    ## Call:
    ## glm(formula = Revenue_Binary ~ Total_Sessions + Conversion_Rate + 
    ##     Avg_Session_Duration, family = binomial, data = mydata)
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)          -0.8493387  0.7351481  -1.155    0.248
    ## Total_Sessions        0.0002231  0.0014133   0.158    0.875
    ## Conversion_Rate       1.1609186  0.7192518   1.614    0.107
    ## Avg_Session_Duration -0.1110208  0.1379476  -0.805    0.421
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 211.70  on 159  degrees of freedom
    ## Residual deviance: 208.39  on 156  degrees of freedom
    ## AIC: 216.39
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
anova(logit_model)
```

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: Revenue_Binary
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                      Df Deviance Resid. Df Resid. Dev
    ## NULL                                   159     211.70
    ## Total_Sessions        1  0.01422       158     211.69
    ## Conversion_Rate       1  2.64650       157     209.04
    ## Avg_Session_Duration  1  0.64962       156     208.39

<p>
The coefficients represent the estimated effect of each predictor
variable on the log-odds of the outcome variable being in the positive
class (1).
</p>
<p>
For example, the coefficient for Total_Sessions is approximately
0.0002231, indicating that for each unit increase in Total_Sessions, the
log-odds of the outcome variable being in the positive class increases
by 0.0002231 units.
</p>
<p>
The coefficients for Conversion_Rate and Avg_Session_Duration are
1.1609186 and -0.1110208, respectively.
</p>

#### Residual Analysis

``` r
# Residual Analysis
residuals(logit_model)
```

    ##          1          2          3          4          5          6          7 
    ##  1.5458998 -0.8492063 -0.8527597 -0.9183062  1.4267194 -1.0028099 -0.8988519 
    ##          8          9         10         11         12         13         14 
    ##  1.3712167  1.5251976 -0.8826919 -0.9987494  1.3762101 -1.1249902 -0.8396524 
    ##         15         16         17         18         19         20         21 
    ## -0.9966884 -1.0265133 -0.9379967 -1.1498749 -0.9308056  1.4658304 -0.9389572 
    ##         22         23         24         25         26         27         28 
    ## -1.1347494 -1.0671331  1.5979272 -0.8158540 -0.9708095  1.2835367  1.3131500 
    ##         29         30         31         32         33         34         35 
    ##  1.4436258  1.3351454 -0.7955089  1.3241214  1.5698692 -1.0765072 -0.7954189 
    ##         36         37         38         39         40         41         42 
    ## -0.8257474  1.4868422 -1.0398369 -0.9040623  1.1404993 -0.9407847 -1.1572207 
    ##         43         44         45         46         47         48         49 
    ## -1.1220745  1.4160402  1.4482367 -0.9592012 -0.8122101  1.3855960 -1.1339354 
    ##         50         51         52         53         54         55         56 
    ## -1.0550974 -1.1002643 -0.9238604 -1.0369594 -1.2293515 -0.9829706  1.4922321 
    ##         57         58         59         60         61         62         63 
    ## -0.9782253 -0.9777151 -0.9353946 -1.0126308 -0.8998333 -1.0979265 -0.8258289 
    ##         64         65         66         67         68         69         70 
    ##  1.1527493 -0.7930225 -1.1401239 -0.8863203 -0.7385364  1.1481455  1.5648910 
    ##         71         72         73         74         75         76         77 
    ##  1.1978600 -1.0386886 -0.8425608 -1.1475154  1.2896632 -0.8182684  1.4109192 
    ##         78         79         80         81         82         83         84 
    ## -0.9859410 -0.8767846  1.6628349 -0.8853418 -0.8905493 -0.8414351  1.3822860 
    ##         85         86         87         88         89         90         91 
    ##  1.2570199  1.3870896  1.6302954 -0.8645277 -1.0013917 -0.9630485  1.1610194 
    ##         92         93         94         95         96         97         98 
    ##  1.6200959  1.3942511 -0.8857336 -1.0918482  1.4470138  1.4292224 -1.2003564 
    ##         99        100        101        102        103        104        105 
    ## -1.1296417 -0.8138223 -0.8545251  1.2331770 -0.9582108  1.2676993 -0.7789846 
    ##        106        107        108        109        110        111        112 
    ## -0.8522443 -1.0443710 -0.8786750  1.2992106  1.2883899 -1.0243540  1.4269058 
    ##        113        114        115        116        117        118        119 
    ##  1.4606106  1.3878572 -0.9575861 -0.9398370  1.3014146  1.2234606 -1.0087927 
    ##        120        121        122        123        124        125        126 
    ##  1.3239776 -0.9846323  1.1997294  1.4330638 -0.7911034 -0.8502464 -0.8576156 
    ##        127        128        129        130        131        132        133 
    ## -1.1601529 -1.1533620 -0.8636970 -0.9352462  1.3022532  1.3410409  1.4971404 
    ##        134        135        136        137        138        139        140 
    ## -0.9486561 -1.0589601  1.4640681 -0.8272578  1.4228100  1.2342499  1.3037887 
    ##        141        142        143        144        145        146        147 
    ##  1.2185530 -0.9381057 -0.8623546  1.2272132 -0.8066662 -1.0242440 -0.9207381 
    ##        148        149        150        151        152        153        154 
    ## -0.8995723  1.4421148 -0.8937857 -1.1065462 -1.0516325 -0.8334256  1.4115464 
    ##        155        156        157        158        159        160 
    ## -0.8917771  1.4272029  1.6942057 -1.1407678 -0.8545774 -0.8905206

``` r
plot(logit_model)
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-27-2.png)<!-- -->![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-27-3.png)<!-- -->![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-27-4.png)<!-- -->
<p>
Function calculates the residuals for the fitted logistic regression
model (logit_model). It returns a vector containing the residuals.
</p>
<p>
Plot() function generates diagnostic plots for the logistic regression
model (logit_model).diagnostic plots including residuals vs. fitted
values, quantile-quantile (Q-Q) plot, and leverage plot
</p>

#### Prediction

``` r
predicted_prob <- predict(logit_model, type = "response")

# Create prediction object
predictions <- prediction(predicted_prob, mydata$Revenue_Binary)

roc_curve <- roc(mydata$Revenue_Binary, predicted_prob)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "red")
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
# Calculate AUC
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")
```

    ## AUC: 0.5915

``` r
# Calculate performance measures
perf <- performance(predictions, "tpr", "fpr")

# Plot ROC curve
plot(perf, main = "ROC Curve", col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "red")
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-28-2.png)<!-- -->

``` r
# Plot ROC curve
plot(perf, main = "ROC Curve", col = "blue", lwd = 2, 
     xlab = "False Positive Rate", ylab = "True Positive Rate", 
     xlim = c(0, 1), ylim = c(0, 1))
abline(a = 0, b = 1, lty = 2, col = "red")  # Diagonal line for reference

# Add AUC value to the plot
auc_value <- performance(predictions, "auc")@y.values[[1]]
text(0.5, 0.5, paste("AUC =", round(auc_value, 2)), col = "#4daf4a", lwd=4)
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-28-3.png)<!-- -->

``` r
# Prediction 
new_data <- mydata[1:10, ]
predictions <- predict(logit_model, newdata = new_data, type = "response")
print(predictions)
```

    ##         1         2         3         4         5         6         7         8 
    ## 0.3027333 0.3027252 0.3048305 0.3440319 0.3614021 0.3951736 0.3323341 0.3905819 
    ##         9        10 
    ## 0.3125115 0.3226536

``` r
hist(predictions, breaks = 20, col = "lightblue", main = "Histogram of Predicted Probabilities")
```

![](Web_Analytics_Study_files/figure-gfm/unnamed-chunk-28-4.png)<!-- -->

#### Model Accuracy

``` r
predicted <- predict(logit_model, type = "response")
predicted_binary <- ifelse(predicted > 0.5, 1, 0)
confusion <- table(predicted_binary, mydata$Revenue_Binary)
accuracy <- sum(diag(confusion)) / sum(confusion)
print(accuracy)
```

    ## [1] 0.6375

<p>
The code reads a dataset from an Excel file, preprocesses it to create a
binary outcome variable based on a threshold, fits a logistic regression
model to predict this outcome using three predictor variables, conducts
residual analysis, evaluates model performance using ROC curve and
calculates AUC, makes predictions for a subset of the data, and assesses
model accuracy metrics including accuracy and precision.
</p>
