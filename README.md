# Exploratory Analysis of UFO Sightings from NUFORC Dataset

## Dataset
The National UFO Reporting Center Online Database
- The NUFORC Databank is the largest independently collected set of UFO / UAP sighting reports available on the internet
- https://nuforc.org/databank/

## Columns
- Sighting Datetime
- Sighting City
- Sighting State
- Sighting Country
- UFO Shape
- Sighting Duration (Seconds)
- Sighting Duration (Hours/Minutes)
- Witness Comments
- Date Posted to NUFORC
- Latitude
- Longitude

## Areas for Investigation
- Where are UFOs most likely to be sighted? Are they in specific countries, certain distances from the equator, or near specific landmarks?
- When are UFOs most likely to be sighted? Are there specific seasons, months, days of the week, or specific days (like national holidays)? What could explain those (fireworks, alcohol, etc.)?
- What are the most common UFO Descriptions? What are the most common shapes, eyewitness accounts, etc.? 

## Graphs
- Maps
- Bar Charts and Line Graphs

# Next Steps
- Separate Time Series Analysis and the actual Time Series stuff
- Specific Days with high UFO sightings, specific months with high UFO sightings
- Our actual "prediction" is going to be a logistic regression model that uses all of these factors to predict the next "UFO sightings"
