# Crime Analysis
Utilizing R, I performed a study on the overall crime levels to gain an understanding if there were any correlations between the types of crimes being committed.

The data being used was from the San Diego Automated Regional Justice Information System (ARJIS) and contains the following columns:
Murder, Rape, Armed Robbery, Strong Arm Robbery, Aggravated Assault, Total Violent Crime, Residential Burglary, Non-Residential Burglary, Total Burglary, Theft >= $400, Theft < $400, Total Thefts, Motor Vehicle Theft, Total Property Crime and Crime Index.

This data is presented as counts of occurrences by month from January 2008 to present day. 

I chose to eliminate each of the crime subcategories and instead focus on the total number of violent crimes, burglaries, thefts and vehicle thefts along with the month they occurred, for a total of five columns. 

Total Violent Crime equals the sum of murders, rapes, robberies, and aggravated assaults.
Total Burglary equals the sum of residential and non-residential burglaries.
Total Thefts equals the sum of thefts >= $400 and thefts < $400.

There are 132 total observations, one per month for 11 years.

The original data can be found here: http://crimestats.arjis.org/default.aspx

## Research Question
My main research question was to study the correlation and overall trends between the total number of violent crimes, burglaries, thefts and vehicle thefts over time to see if they have remained the same.

### Report
The full report and findings can be found here: https://ericborn.github.io/crime/
