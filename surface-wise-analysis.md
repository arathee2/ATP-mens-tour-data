Surface wise Analysis
================

We will divide our data into subsets containing hard, grass and clay match separately.

``` r
library(ggplot2)
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
atp <- read.csv("Data.csv")
str(atp)
```

    ## 'data.frame':    46652 obs. of  54 variables:
    ##  $ ATP       : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Location  : Factor w/ 111 levels "'s-Hertogenbosch",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ Tournament: Factor w/ 233 levels "AAPT Championships",..: 19 19 19 19 19 19 19 19 19 19 ...
    ##  $ Date      : Factor w/ 4037 levels "1/01/2001","1/01/2003",..: 2984 2984 2984 2984 2984 2984 2984 2984 2984 2984 ...
    ##  $ Series    : Factor w/ 9 levels "ATP250","ATP500",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ Court     : Factor w/ 2 levels "Indoor","Outdoor": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Surface   : Factor w/ 4 levels "Carpet","Clay",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ Round     : Factor w/ 9 levels "0th Round","1st Round",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Best.of   : int  3 3 3 3 3 3 3 3 3 3 ...
    ##  $ Winner    : Factor w/ 879 levels " Hajek J.","Abel M.",..: 200 221 225 234 255 264 302 332 340 460 ...
    ##  $ Loser     : Factor w/ 1383 levels " Hajek J.","Abdulla M.",..: 724 221 63 630 1344 53 552 73 1345 1204 ...
    ##  $ WRank     : Factor w/ 588 levels "1","10","100",..: 499 425 342 505 552 474 197 25 175 56 ...
    ##  $ LRank     : Factor w/ 935 levels "1","10","100",..: 805 629 716 865 230 28 576 254 110 405 ...
    ##  $ W1        : int  6 6 6 6 7 3 6 6 6 7 ...
    ##  $ L1        : int  4 3 7 1 6 6 2 4 3 6 ...
    ##  $ W2        : int  6 6 7 6 5 7 6 7 2 6 ...
    ##  $ L2        : int  2 3 5 4 7 6 1 6 6 7 ...
    ##  $ W3        : int  NA NA 6 NA 6 6 NA NA 6 6 ...
    ##  $ L3        : int  NA NA 3 NA 4 4 NA NA 1 4 ...
    ##  $ W4        : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ L4        : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ W5        : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ L5        : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Wsets     : int  2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Lsets     : int  0 0 1 0 1 1 0 0 1 1 ...
    ##  $ Comment   : Factor w/ 12 levels "Compleed","Completed",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ CBW       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ CBL       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ GBW       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ GBL       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ IWW       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ IWL       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ SBW       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ SBL       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ B365W     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ B365L     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ B.WW      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ B.WL      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ EXW       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ EXL       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ PSW       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ PSL       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ WPts      : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ LPts      : Factor w/ 2454 levels "","1","10","100",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ UBW       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ UBL       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ LBW       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ LBL       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ SJW       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ SJL       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ MaxW      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ MaxL      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ AvgW      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ AvgL      : num  NA NA NA NA NA NA NA NA NA NA ...

``` r
prop.table(table(atp$Surface))
```

    ## 
    ##     Carpet       Clay      Grass       Hard 
    ## 0.03631141 0.33070394 0.11137786 0.52160679

``` r
# Dividing data according to surfaces. Hard and carpet are counted in hard courts.

atp_clay <- atp %>% filter(atp$Surface == "Clay")
atp_hard <- atp %>% filter(atp$Surface == "Hard" | atp$Surface == "Carpet")
atp_grass <- atp %>% filter(atp$Surface == "Grass")
```

Hard Court Analysis
===================

``` r
# hard/carpet court win loss ratio


name <- as.factor(levels(atp_hard$Winner))
wins <- rep(0,length(name))
losses = rep(0,length(name))
win_loss_hard <- data.frame(name, wins, losses )


for (i in win_loss_hard$name) {
    
    for (j in atp_hard$Winner) {
        
        if(i == j){
            
            
            win_loss_hard$wins[win_loss_hard$name == i] <- win_loss_hard$wins[win_loss_hard$name == i] + 1
        }
        
    } 
    
    for (j in atp_hard$Loser) {
        
        if(i == j){
            
            
            win_loss_hard$losses[win_loss_hard$name == i] <- win_loss_hard$losses[win_loss_hard$name == i] + 1
        }
        
    } 
    
}

win_loss_hard$name[win_loss_hard$losses == 0 | win_loss_hard$wins == 0] <- NA

# Minimume wins required to be in list is 100

win_loss_top_hard <- na.omit(win_loss_hard) %>% mutate(win_loss_ratio = wins/losses) %>% filter(wins >= 100) %>% arrange(desc(win_loss_ratio)) %>% top_n(6)
```

    ## Selecting by win_loss_ratio

``` r
ggplot(data = win_loss_top_hard,aes(y = win_loss_ratio, x = name )) + geom_bar(stat = "identity", fill = "#2A628F") + labs(title = "Win/Loss Ratio in Hard/Carpet Courts", x = "Player", y = "Win/Loss Ratio") + scale_y_continuous(breaks = seq(0,6,0.3))
```

![](surface-wise-analysis-github-output_files/figure-markdown_github/unnamed-chunk-2-1.png)

We can see that Djokovic dominatee the hard court although Federer is not far behind. Winner : Novak Djokovic

Grass court Analysis
====================

``` r
# grass matches win loss ratio

name <- as.factor(levels(atp_grass$Winner))
wins <- rep(0,length(name))
losses = rep(0,length(name))
win_loss_grass <- data.frame(name, wins, losses )


for (i in win_loss_grass$name) {
    
    for (j in atp_grass$Winner) {
        
        if(i == j){
            
            
            win_loss_grass$wins[win_loss_grass$name == i] <- win_loss_grass$wins[win_loss_grass$name == i] + 1
        }
        
    } 
    
    for (j in atp_grass$Loser) {
        
        if(i == j){
            
            
            win_loss_grass$losses[win_loss_grass$name == i] <- win_loss_grass$losses[win_loss_grass$name == i] + 1
        }
        
    } 
    
}

win_loss_grass$name[win_loss_grass$losses == 0 | win_loss_grass$wins == 0] <- NA

# Minimume wins required to be in list is 20 since matches played in grass courts are low in number.

win_loss_top_grass <- na.omit(win_loss_grass) %>% mutate(win_loss_ratio = wins/losses) %>% filter(wins >= 20) %>% arrange(desc(win_loss_ratio)) %>% top_n(6)
```

    ## Selecting by win_loss_ratio

``` r
ggplot(data = win_loss_top_grass,aes(y = win_loss_ratio, x = name )) + geom_bar(stat = "identity", fill = "#00DB5F") + labs(title = "Win/Loss Ratio in grass courts", x = "Player", y = "Win/Loss Ratio") + scale_y_continuous(breaks = seq(0,8,0.4))
```

![](surface-wise-analysis-github-output_files/figure-markdown_github/unnamed-chunk-3-1.png)

Federer dominates the grass courts by a significant margin as expected. Winner: Roger Federer

Clay courts Analysis
====================

``` r
# clay court matches win loss ratio

name <- as.factor(levels(atp_clay$Winner))
wins <- rep(0,length(name))
losses = rep(0,length(name))
win_loss_clay <- data.frame(name, wins, losses )


for (i in win_loss_clay$name) {
    
    for (j in atp_clay$Winner) {
        
        if(i == j){
            
            
            win_loss_clay$wins[win_loss_clay$name == i] <- win_loss_clay$wins[win_loss_clay$name == i] + 1
        }
        
    } 
    
    for (j in atp_clay$Loser) {
        
        if(i == j){
            
            
            win_loss_clay$losses[win_loss_clay$name == i] <- win_loss_clay$losses[win_loss_clay$name == i] + 1
        }
        
    } 
    
}

win_loss_clay$name[win_loss_clay$losses == 0 | win_loss_clay$wins == 0] <- NA

# Minimum wins required to be in the list is 70.

win_loss_top_clay <- na.omit(win_loss_clay) %>% mutate(win_loss_ratio = wins/losses) %>% filter(wins >= 70) %>% arrange(desc(win_loss_ratio)) %>% top_n(6)
```

    ## Selecting by win_loss_ratio

``` r
ggplot(data = win_loss_top_clay,aes(y = win_loss_ratio, x = name )) + geom_bar(stat = "identity", fill = "#BE5A38") + labs(title = "Win/Loss Ratio in clay courts", x = "Player", y = "Win/Loss Ratio") + scale_y_continuous(breaks = seq(0,10.5,0.5))
```

![](surface-wise-analysis-github-output_files/figure-markdown_github/unnamed-chunk-4-1.png)

Nadal shines here. He significantly dominates the others on clay. No one is even near him. Winner: Rafael Nadal

Conclusion: Nadal dominates clay more than Federer dominates grass more than Djokovic dominates hard courts.
