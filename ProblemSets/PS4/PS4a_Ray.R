# PS4a - JSON Exercise

# (a) Download the JSON file
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"')

# (b) Print the file to console
system('cat dates.json')

# (c) Convert to dataframe
library(jsonlite)
library(tidyverse)

mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

# (d) Check object types
print(class(mydf))
print(class(mydf$date))

# (e) List first 6 rows
print(head(mydf))
