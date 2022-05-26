LIBNAME Week8 '/home/u59262643/Week8';

* Problem 1 Import the data as house_price;
DATA house_price;
	INFILE '/home/u59262643/Week8/Bay Area House Price.csv' DSD FIRSTOBS=2;
	INPUT address :$45. info :$100. z_address :$30. bathrooms bedrooms finishedsqft lastsolddate 
	lastsoldprice latitude longitude neighborhood :$50. totalrooms usecode :$20. yearbuilt $ zestimate zipcode $ zpid;
	INFORMAT lastsolddate MMDDYY10.;
RUN;

PROC PRINT DATA=house_price (OBS=5);
TITLE 'Problem 1: Data Input';
RUN;


* Problem 2 Drop Variables;
DATA house_price_data (DROP= address info z_address neighborhood latitude longitude zpid);
	SET house_price;
RUN;

PROC PRINT DATA=house_price_data (OBS=5);
TITLE 'Problem 2: Drop Tables (Data Statement)';
RUN;

PROC SQL;
ALTER TABLE house_price
DROP COLUMN address, info, z_address, neighborhood, latitude, longitude, zpid;
QUIT;

PROC PRINT DATA=house_price (OBS=5);
TITLE 'Problem 2: Drop Tables (SQL)';
RUN;


* Problem 3 Add a new variable;
DATA house_price_data;
	SET house_price_data;
	price_per_square_foot = lastsoldprice/finishedsqft;
RUN;

PROC PRINT DATA=house_price_data (OBS=5);
TITLE 'Problem 3: Add Price Per Square Foot (Data Statement)';
RUN;

PROC SQL;
ALTER TABLE house_price
ADD price_per_square_foot FLOAT;
UPDATE house_price
SET price_per_square_foot = lastsoldprice/finishedsqft;
QUIT;

PROC PRINT DATA=house_price (OBS=5);
TITLE 'Problem 3: Add Price Per Square Foot (SQL)';
RUN;


* Problem 4 Average of Last Sold Price by Zipcode;
PROC MEANS DATA = house_price_data MEAN;
TITLE 'Problem 4: Average of Last Sold Price by Zipcode (Data Statement)';
CLASS zipcode;
VAR lastsoldprice;
RUN;

PROC SQL;
TITLE 'Problem 4: Average of Last Sold Price by Zipcode (SQL)';
SELECT AVG(lastsoldprice) AS AveragePrice, zipcode
FROM house_price
GROUP BY zipcode;
QUIT;


* Problem 5 Average of Last Sold Price, Total Rooms, and Bedrooms by Usecode;
PROC MEANS DATA = house_price_data MEAN;
TITLE 'Problem 5: Average of Last Sold Price, Total Rooms, and Bedrooms by Usecode (Data Statement)';
CLASS usecode;
VAR lastsoldprice totalrooms bedrooms;
RUN;

PROC SQL;
TITLE 'Problem 5: Average of Last Sold Price, Total Rooms, and Bedrooms by Usecode (SQL)';
SELECT AVG(lastsoldprice) AS AveragePrice, AVG(totalrooms) AS AverageRooms, AVG(bedrooms) AS AverageBedrooms, usecode
FROM house_price
GROUP BY usecode;
QUIT;


* Problem 6 Plot a Bar Chart for Bathrooms, Bedrooms, Usecode, and Total Rooms;
PROC SGPLOT DATA=house_price;
TITLE 'Problem 6: Frequency of Bathrooms';
VBAR bathrooms;
RUN;

ODS LISTING GPATH='/home/u59262643/Week8';
ODS GRAPHICS / RESET
	IMAGENAME = 'bedrooms'
	OUTPUTFMT = PNG
	HEIGHT = 4IN WIDTH = 5IN;
	
PROC SGPLOT DATA=house_price;
TITLE 'Frequency of Bedrooms';
VBAR bedrooms;
RUN;
ODS LISTING CLOSE;

PROC SGPLOT DATA=house_price;
TITLE 'Frequency of Usecode';
VBAR usecode;
RUN;

PROC SGPLOT DATA=house_price;
TITLE 'Frequency of Total Rooms';
VBAR totalrooms;
RUN;


* Problem 7 Plot a Histogram and Boxplot for Last Sold Price and Zestimate;
* Answer: They are skewed. The Median for lastsoldprice is 990,000 and the Median
for Zestimate is 1,230,758;
PROC SGPLOT DATA=house_price;
TITLE 'Problem 7: Histogram of Last Sold Price';
HISTOGRAM lastsoldprice;
RUN;

PROC SGPLOT DATA=house_price;
TITLE 'Problem 7: Box Plot of Last Sold Price';
VBOX lastsoldprice;
RUN;

PROC SGPLOT DATA=house_price;
TITLE 'Problem 7: Histogram of Zestimate';
HISTOGRAM zestimate;
RUN;

PROC SGPLOT DATA=house_price;
TITLE 'Problem 7: Box Plot of Zestimate';
VBOX zestimate;
RUN;

PROC MEANS DATA=house_price MEDIAN;
TITLE 'Problem 7: Median of Last Sold Price';
VAR lastsoldprice;
RUN;

PROC MEANS DATA=house_price MEDIAN;
TITLE 'Problem 7: Median of Zestimate';
VAR zestimate;
RUN;


* Problem 8 Compare the Average Zestimate for any two Zipcodes;
* Answer: I disagree. While the variances are statistically similar, the Pooled method
result of the t-test is 0.0001, meaning the zestimates between the zipcodes differ statistically;
DATA twozips;
SET house_price;
WHERE zipcode = '94103' OR zipcode = '94107';
RUN;


PROC TTEST DATA=twozips;
TITLE 'Problem 8: T-Test Between Zipcodes 94103 and 94107';
CLASS zipcode;
VAR zestimate;
RUN;


* Problem 9 Analysis of zestimate and lastsoldprice;
* Answer: Disagree. The P-Value of <.0001 indicates there is a statistical difference between the zestimate
and the last sold price;
PROC TTEST;
TITLE 'Problem 9: T-Test Between Zestimate and Last Sold Price';
	PAIRED zestimate * lastsoldprice;
RUN;


* Problem 10 Analysis of Bedrooms Association with Usecode;
* Answer: Yes, the number of bedrooms is associated with usecode. While usecode is a character type,
we can see the mean value changes significantly with each different usecode;
PROC SORT DATA=house_price OUT=house_price_sorted;
	BY usecode;
RUN;

PROC CORR DATA=house_price_sorted;
TITLE 'Problem 10: Correlation of Bedrooms with Usecode';
	VAR bedrooms;
	BY usecode;
RUN;

* Problem 11 Analysis of Bedrooms Association with Bathrooms;
* Answer: Yes, according to the correlation coefficient, the number of bedrooms has a
strong positive correlation with the number of bathrooms;
PROC CORR DATA=house_price PLOTS(MAXPOINTS=none)=scatter;
TITLE 'Problem 11: Correlation of Bedrooms with Bathrooms';
	VAR bedrooms;
	WITH bathrooms;
RUN;


* Problem 12 Calculate correlation coefficients of all numerical variables with Zestimate;
PROC CORR DATA=house_price PLOTS(MAXPOINTS=none)= (scatter matrix);
TITLE 'Problem 12: Correlation Coefficients of All Numerical Variables with Zestimate';
	WITH zestimate;
RUN;


* Problem 13 Find a Regression Model for Zestimate with the first three most correlated variables
(lastsoldprice, finishedsqft, bathrooms);
PROC REG DATA=house_price PLOTS(MAXPOINTS=none) OUTEST=threevar;
	MODEL zestimate= lastsoldprice finishedsqft bathrooms;
	TITLE 'Problem 13: Multiple Linear Regression with Top 3 Correlated Variables';
RUN;


* Problem 14 Find a Regression Model for Zestimate with the first five most correlated variables
(lastsoldprice, finishedsqft, bathrooms, bedrooms, totalrooms);
PROC REG DATA=house_price PLOTS(MAXPOINTS=none) OUTEST=fivevar;
	MODEL zestimate= lastsoldprice finishedsqft bathrooms bedrooms totalrooms;
	TITLE 'Problem 14: Multiple Linear Regression with Top 3 Correlated Variables';
RUN;


* Problem 15 Compare the Adjusted R^2 Values from the Previous Two Models;
* Answer: The adjusted R^2 value using 3 variables (problem 13) is 0.8319, while the adjusted R^2
value using 5 variables (problem 14) is 0.8328. While problem 14's model is technically better,
the increase is miniscule.


* Problem 16 Use the better model to predict the house prices given values of the independent variables;
DATA new_house;
	INPUT lastsoldprice finishedsqft bathrooms bedrooms totalrooms;
	DATALINES;
2550000 2050 3 3 9
1300000 1325 1 2 5
5640000 5100 5.5 7 13
990000 1120 2 2 4
;
RUN;

PROC SCORE DATA= new_house SCORE=fivevar OUT=zestPred type=parms nostd predict;
	VAR lastsoldprice finishedsqft bathrooms bedrooms totalrooms;
RUN;

PROC PRINT DATA=zestPred;
TITLE 'Problem 16: Predicted Zestimate for House';
RUN;


* Problem 17 Export the Model to 'Prediction.xlsx';
PROC EXPORT DATA=zestPred 
	DBMS=xlsx
	OUTFILE='/home/u59262643/Week8/Prediction.xlsx'
	REPLACE;
RUN;


* Problem 18 Create the Macro 'average';
%MACRO average(category=, price=);
	PROC MEANS DATA=house_price MEAN NOPRINT;
	CLASS &category;
	VAR &price;
	OUTPUT OUT=averageprice (drop=_type_ _freq_) MEAN=;
	RUN;
	PROC PRINT DATA=averageprice;
		TITLE "Problems 19/20: Average of &price by &category.";
	RUN;
%MEND average;


* Problem 19 Call the Macro for category=zipcode and price=price_per_square_foot;
%average(category=zipcode, price=price_per_square_foot);


* Problem 20 Call the Macro for category=totalrooms and price=zestimate;
%average(category=totalrooms, price=zestimate);