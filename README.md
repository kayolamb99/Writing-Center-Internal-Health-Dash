# Writing-Center-Internal-Health-Dash

This project was undertaken primarily to better understand and visualize the top-line metrics valuable to Writing Center management. 

## Data & Processing

Obtained data from myWCOnline's internal system statistics feature, which loads a csv file directly to a your local machine. Cleaned data tables primairly using dplyr, with some work done using the sqldf package, both within R. This work can be found in the various \[datafile name]\_cleaning.R' files

One particular issue of note would be the process by which I processed the 'client goals column.' As this column was a complex data structure, with both categories and counts of goals worked on within the session rolled into one variable, its separation was placed into a separate file named 'Analysis'

## Visualizations

For practical purposes, all visualization is done using Tableau 2020. This was done for ease of ditribution to Writing Center management, as well as to onboard my collaborator as easily as possible. 

Our first iteration of the analysis focused on top-line metrics important to the Writing Center, such as appointment visitations, as well as valuable demographic insights such as client's first language. This presentation can be found [here](https://docs.google.com/presentation/d/1Yn7pr302tnqr5H0UzcD54fqC35ga0L4NL2PtyZw7ZG0/edit?usp=sharing).
