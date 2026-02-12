-- Read in the Florida insurance data
.mode csv
.import FL_insurance_sample.csv fl_insurance

-- Print the first 10 rows
SELECT * FROM fl_insurance LIMIT 10;

-- List unique counties
SELECT DISTINCT county FROM fl_insurance;

-- Average property appreciation from 2011 to 2012
SELECT AVG(CAST(tiv_2012 AS REAL) - CAST(tiv_2011 AS REAL)) AS avg_appreciation
FROM fl_insurance;

-- Frequency table of construction variable
SELECT construction, COUNT(*) AS count,
       ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM fl_insurance), 2) AS percentage
FROM fl_insurance
GROUP BY construction;
