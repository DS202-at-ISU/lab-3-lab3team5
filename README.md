
# Lab report \#3 - instructions

Follow the instructions posted at
<https://ds202-at-isu.github.io/labs.html> for the lab assignment. The
work is meant to be finished during the lab time, but you have time
until Monday evening to polish things.

Include your answers in this document (Rmd file). Make sure that it
knits properly (into the md file). Upload both the Rmd and the md file
to your repository.

All submissions to the github repo will be automatically uploaded for
grading once the due date is passed. Submit a link to your repository on
Canvas (only one submission per team) to signal to the instructors that
you are done with your submission.

# Lab 3: Avenger’s Peril

## As a team

Extract from the data below two data sets in long form `deaths` and
`returns`

``` r
av <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/avengers/avengers.csv", stringsAsFactors = FALSE)
head(av)
```

    ##                                                       URL
    ## 1           http://marvel.wikia.com/Henry_Pym_(Earth-616)
    ## 2      http://marvel.wikia.com/Janet_van_Dyne_(Earth-616)
    ## 3       http://marvel.wikia.com/Anthony_Stark_(Earth-616)
    ## 4 http://marvel.wikia.com/Robert_Bruce_Banner_(Earth-616)
    ## 5        http://marvel.wikia.com/Thor_Odinson_(Earth-616)
    ## 6       http://marvel.wikia.com/Richard_Jones_(Earth-616)
    ##                    Name.Alias Appearances Current. Gender Probationary.Introl
    ## 1   Henry Jonathan "Hank" Pym        1269      YES   MALE                    
    ## 2              Janet van Dyne        1165      YES FEMALE                    
    ## 3 Anthony Edward "Tony" Stark        3068      YES   MALE                    
    ## 4         Robert Bruce Banner        2089      YES   MALE                    
    ## 5                Thor Odinson        2402      YES   MALE                    
    ## 6      Richard Milhouse Jones         612      YES   MALE                    
    ##   Full.Reserve.Avengers.Intro Year Years.since.joining Honorary Death1 Return1
    ## 1                      Sep-63 1963                  52     Full    YES      NO
    ## 2                      Sep-63 1963                  52     Full    YES     YES
    ## 3                      Sep-63 1963                  52     Full    YES     YES
    ## 4                      Sep-63 1963                  52     Full    YES     YES
    ## 5                      Sep-63 1963                  52     Full    YES     YES
    ## 6                      Sep-63 1963                  52 Honorary     NO        
    ##   Death2 Return2 Death3 Return3 Death4 Return4 Death5 Return5
    ## 1                                                            
    ## 2                                                            
    ## 3                                                            
    ## 4                                                            
    ## 5    YES      NO                                             
    ## 6                                                            
    ##                                                                                                                                                                              Notes
    ## 1                                                                                                                Merged with Ultron in Rage of Ultron Vol. 1. A funeral was held. 
    ## 2                                                                                                  Dies in Secret Invasion V1:I8. Actually was sent tto Microverse later recovered
    ## 3 Death: "Later while under the influence of Immortus Stark committed a number of horrible acts and was killed.'  This set up young Tony. Franklin Richards later brought him back
    ## 4                                                                               Dies in Ghosts of the Future arc. However "he had actually used a hidden Pantheon base to survive"
    ## 5                                                      Dies in Fear Itself brought back because that's kind of the whole point. Second death in Time Runs Out has not yet returned
    ## 6                                                                                                                                                                             <NA>

``` r
View(av)
```

Get the data into a format where the five columns for Death\[1-5\] are
replaced by two columns: Time, and Death. Time should be a number
between 1 and 5 (look into the function `parse_number`); Death is a
categorical variables with values “yes”, “no” and ““. Call the resulting
data set `deaths`.

Similarly, deal with the returns of characters.

Based on these datasets calculate the average number of deaths an
Avenger suffers.

``` r
# Deaths

deaths <- av %>% 
  pivot_longer(
    starts_with("Death"),
    names_to = "Time",
    values_to = "Died"
  ) %>% 
  select(
    URL,
    Time, Died
  ) %>% 
  mutate(
    Time = parse_number(Time)
    ) %>%
  group_by(URL,Died) %>%
  summarise(deaths = max(Time)) %>%
  filter(URL != "") %>%
  filter(Died != "", Died != "NO")
```

    ## `summarise()` has grouped output by 'URL'. You can override using the `.groups`
    ## argument.

``` r
# Calculate the average number of deaths an Avenger suffers
# Filter out blank values and count "yes" entries
totalDeath = sum(deaths$deaths)
AverageDeath = mean(deaths$deaths)
totalDeath
```

    ## [1] 89

``` r
AverageDeath
```

    ## [1] 1.289855

``` r
# returns 
returnTimes <- av %>% 
  pivot_longer(
    starts_with("Return"),
    names_to = "Time",
    values_to = "Return"
  ) %>% 
  select(
    URL,
    Time, Return
  ) %>% 
  mutate(
    Time = parse_number(Time)
    ) %>%
  group_by(URL,Return) %>%
  summarise(returns = max(Time)) %>%
  filter(URL != "") %>%
  filter(Return != "")
```

    ## `summarise()` has grouped output by 'URL'. You can override using the `.groups`
    ## argument.

``` r
View(returnTimes)
```

# Individually

For each team member, copy this part of the report.

## Alister

Each team member picks one of the statements in the FiveThirtyEight
[analysis](https://fivethirtyeight.com/features/avengers-death-comics-age-of-ultron/)
and fact checks it based on the data. Use dplyr functionality whenever
possible.

### FiveThirtyEight Statement

Out of 173 listed Avengers, my analysis found that 69 had died at least
one time after they joined the team

### Include the code

Make sure to include the code to derive the (numeric) fact for the
statement

``` r
# Calculate the number of unique Avengers who have died at least once
unique_deaths <- deaths %>%
  filter(deaths >= 1, Died == "YES") %>%
  summarise(died_at_least_once = n_distinct(URL))

# Calculate the total number of Avengers from the original dataset
total_avengers <- av %>%
  summarise(total = n_distinct(URL))

# Display the results
cat("Total Avengers:", total_avengers$total, "\n")
```

    ## Total Avengers: 173

``` r
cat("Avengers who died at least once:", sum(unique_deaths$died_at_least_once), "\n")
```

    ## Avengers who died at least once: 69

### Include your answer

Include at least one sentence discussing the result of your
fact-checking endeavor.

## Brietta

Each team member picks one of the statements in the FiveThirtyEight
[analysis](https://fivethirtyeight.com/features/avengers-death-comics-age-of-ultron/)
and fact checks it based on the data. Use dplyr functionality whenever
possible.

### FiveThirtyEight Statement

> 57 occasions the individual made a comeback.

### Code

``` r
returnTimes = returnTimes %>% 
  filter(Return == "YES")

sum(returnTimes$returns)
```

    ## [1] 57

### Answer

My code shows that 57 times the avengers had a return after dying, which
is exactly the same number that the articles claimed for number of
occasions that someone came back after dying.

## Brianna

Each team member picks one of the statements in the FiveThirtyEight
[analysis](https://fivethirtyeight.com/features/avengers-death-comics-age-of-ultron/)
and fact checks it based on the data. Use dplyr functionality whenever
possible.

### FiveThirtyEight Statement

> Quote the statement you are planning to fact-check.

### Include the code

Make sure to include the code to derive the (numeric) fact for the
statement

### Include your answer

Include at least one sentence discussing the result of your
fact-checking endeavor.

Upload your changes to the repository. Discuss and refine answers as a
team.
