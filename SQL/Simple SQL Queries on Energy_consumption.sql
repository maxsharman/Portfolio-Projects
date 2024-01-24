-- Viewing energy_consumption data set
SELECT *
FROM energy_consumption;

-- Renewable Energy Ratio
SELECT (SUM(RenewableEnergy) / SUM(EnergyConsumption)) AS RenewableEnergyRatio 
FROM energy_consumption;

-- Average Energy Consumption
SELECT AVG(EnergyConsumption) as AvgEnergyConsumption 
FROM energy_consumption;

-- Average Temperature
SELECT AVG(Temperature) as AvgTemperature 
FROM energy_consumption;

-- Minimum and Maxinum Energy Consumption
SELECT MAX(EnergyConsumption) AS MaxEnergy, MIN(EnergyConsumption) as MinEnergy
FROM energy_consumption;

-- Total Energy Consumption
SELECT SUM(EnergyConsumption) as TotalEnergyConsumption
FROM energy_consumption
WHERE TimeStamp >= '01/01/2022 00:00' AND TimeStamp <= '01/01/2022 23:00';

-- Grouping selected columns averages by DayOfWeek
SELECT AVG(Humidity) as AvgHumidity, AVG(Temperature) as AvgTemperature, AVG(EnergyConsumption) as AvgEnergyConsumption, AVG(RenewableEnergy) as AvgRenewableEnergy, DayofWeek
FROM energy_consumption
GROUP BY DayOfWeek;

-- Ordering by AvgTemperature
SELECT DayOfWeek, AVG(Temperature) as AvgTemperature
FROM energy_consumption
GROUP BY DayOfWeek
ORDER BY AvgTemperature;

-- Limiting the output of data
SELECT Temperature
FROM energy_consumption
LIMIT 10;

-- Joining Timestamp and EnergyConsumption
-- to find the timestamp where the maximum and minimum temperature occurs

	SELECT ec.Timestamp, ec.EnergyConsumption as Max_Energy
FROM energy_consumption as ec
JOIN ( 
	SELECT MAX(EnergyConsumption) AS Max_Energy
    FROM energy_consumption )
   subquery ON ec.EnergyConsumption = subquery.Max_Energy;
   
   SELECT ec.Timestamp, ec.EnergyConsumption as Min_Energy
FROM energy_consumption as ec
JOIN ( 
	SELECT MIN(EnergyConsumption) AS Min_Energy
    FROM energy_consumption )
   subquery ON ec.EnergyConsumption = subquery.Min_Energy;

-- Filtering Temperature within the range of 20-25
SELECT * FROM energy_consumption 
WHERE Temperature BETWEEN 20 AND 25
ORDER BY Temperature DESC;

-- Filtering energy_consumption based on DayOfWeek
SELECT * FROM energy_consumption
WHERE DayOfWeek = 'monday'
ORDER BY Temperature DESC;

-- Filtering energy_consumption based on DayOfWeek with multiple conditions
SELECT * FROM energy_consumption 
WHERE DayOfWeek in ('monday', 'friday')
ORDER BY Temperature DESC;

-- Filtering energy_consumption where Occupancy is less than 5
SELECT * FROM energy_consumption
WHERE Occupancy < 5
ORDER BY Occupancy DESC;

-- Filtering energy_consumption where Occupancy is equal to 5 and DayOfWeek is 'monday'
SELECT * FROM energy_consumption
WHERE Occupancy = 5 and DayOfWeek = 'monday'
ORDER BY Occupancy DESC;



