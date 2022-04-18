# Anthony Weidner GSoC 2022 Animated Interactive ggplots
Working with animint2, ggplots, and multiple data sets & images.

## Easy Tests
I tested out plot analysis and graphs on multiple data sets using skills learned from the animint2 Manual. I uploaded my visualizations to the web using animint2gist(). 
[Source Code and Experimentation](https://github.com/AnthonyWeidner/rstats-gsoc2022-animint2-ggplots/blob/main/Easy%20Tests.Rmd) | [Further Experimentation, Bug Fixing, and Testing](https://github.com/AnthonyWeidner/rstats-gsoc2022-animint2-ggplots/tree/main/Easy%20Tests%20and%20Further%20Testing) 

[Here](http://bl.ocks.org/AnthonyWeidner/raw/b4fea91dbd238e16c4409b6d30aad0bb/) is an example of an animint published using animint2gist(). I then reversed and changed the graph [here](http://bl.ocks.org/AnthonyWeidner/raw/92002723f587bebeb1448049dbc189da/) as part of the exercises in the animint manual. 

I also developed an interactive shinyDashboard app as part of visualizing University of California data using ggplots, allowing the user to change which data is presented. [Interactive UI Application Source Code](https://github.com/AnthonyWeidner/rstats-gsoc2022-animint2-ggplots/blob/main/Interactive%20UI%20Program.Rmd)

## Medium Test
I implemented and translated flip.coin() into an animint. [Animation of coin flipping simulation](http://bl.ocks.org/AnthonyWeidner/raw/1b5acc50d6c8a70dc8dc037338593efe/) | [My Source Code](https://github.com/AnthonyWeidner/rstats-gsoc2022-animint2-ggplots/blob/main/Medium%20Test%20flip.coin()%20implementation%20.Rmd)

## Hard Test
I wrote some testthat unit tests based on my Animint visualization from the Medium Test. For instance, I wanted to test whether animint2gist() was actually returning an object of class "gist." I am also working on testing whether the number of coin flips done actually is interpreted correctly as 100 by using expect_identical(). I used the various testthat resources available on the animint GitHub. [Hard Test Source Code and Experimentation](https://github.com/AnthonyWeidner/rstats-gsoc2022-animint2-ggplots/blob/main/Hard%20Test%20(Unit%20Testing).Rmd)
