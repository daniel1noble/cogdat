# cogdat

Package containing a single function which can be used to process datasets collected on learning or cognition data from horizontal format to vertical format for analysis. Data needs to be in a specific format for the `processDat` function to work correctly. 

## How to set up data

Data needs to be set up with some rules in mind. First, every data must contain an `ID`, `Date` and `var` column label. It is important to note that these NEED to be placed at specfific locations in the data set. An example data set below should better demonstrate this point:

|      |          |          |          |            |          |          |          |          |          |          |  
|------|----------|----------|----------|------------|----------|----------|----------|----------|----------|----------|----|
|      |          |          |          | **Date**        |10.11.12 | 11.11.12 | 12.11.12 | 13.11.12 | 14.11.12 | 15.11.12 | ….
|	   |		  |			  |			  |Trial      | 1       | 2       | 3       | 4       | 5       | 6       | …. |
|      |          |          |          | Task       | 1        | 1        | 1        | 1        | 1        | 1        | …. |
| **ID**   | Sex      | Chan     | trt      | **var**        |          |          |          |          |          |          |    |
| 12   | M        | 1        | 2        | correct    | 1        | 1        | 1        | 0        | 0        | 0        | …. |
|      |          |          |          | latency    | 12       | 2        | 34       | 5        | 6        | 7        | …. |
|      |          |          |          | researcher | FK       | FK       | FK       | FK       | FK       | FK       | …. |
| 34   | F        | 1        | 2        | correct    | 1        | 1        | 1        | 0        | 0        | 0        | …. |
|      |          |          |          | latency    | 12       | 2        | 34       | 5        | 6        | 7        | …. |
|      |          |          |          | researcher | FK       | FK       | FK       | FK       | FK       | FK       | …. |
| ….   | ….       | ….       | ….       | ….         | ….       | ….       | ….       | ….       | ….       | ….       | …. |


Above is an example of how one would set up their data frame. The important structure here is that **Date** is in the 1st row, **ID** in the first column and **var** between trial data and all the individual data. This is important because these markers are used as reference points to define the boundaries of the data set so that all the relevant data can be extracted, re-shuffled and put back together in the right format. Something one should look out for are extra spaces (make sure that there are no rogue spaces in "empty boxes"). Best way to be sure that isn't the case is to simplu put `NA` in the empty cells. So long as you stick to the rules above you ca have as many unique ID's in the data as you want, as many trial specific data (i.e. Trial, Task etc etc), as many unqiue variables for individuals (i.e. correct, latency, researcher etc) and as many trials as your heart desires! 

## Installaton and running `processDat`

Package installation can be done with `devtools`, however, given that it's only just a single function one can just copy the function into R and run it. As an example of how to process data consider the example above (which is an example data file). First, we must import the data file by navigating to our working directory, starting R (or setting our working directory to the location where our data is located) and loading the non-processed data file as follows:

```data <- read.csv("data/dataEg1.csv", header = FALSE, stringsAsFactors = FALSE)```

Note that we set the `header` argument as false. This is done so that **Data** is located in the first time of the imported data file. This must always be the case. Also, for simplicity I have treated all string vectors as characters, not factors, but this doesn't need to be the case. Once the data is imported in R we can processes by calling `processDat` and placing our data frame as the only argument:

``` data_proc <- processDat(data)```

Now the data is formatted vertically and you can change the variables to the revelant types (numeric, factors, integer etc) and it will be ready for analysis. 

**Note**: This function is in very early stages yet and one needs to be careful to correctly check the output with the raw data that was put in to make sure everything matches. I have NOT implemented tests or proper warning messages at this stage. If you have questions, concerns and/or suggestions for improvement please let me know.