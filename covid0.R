
# We manually found the following link via the Web browser's Developer Tools in the Network tab.
# We found the json files

u = "https://static01.nyt.com/newsgraphics/2020/03/16/coronavirus-maps/a3c88fc852744c0bdb9ff9ee092cc5db785705d6/data/timeseries/en/USA.json"

tt = getURLContent(u, verbose = TRUE, followlocation = TRUE)

library(RJSONIO)
us = fromJSON(tt)


# The URL for the JSON changes each day. So today's URL, we want to find it in the current day's page.
# We get the top-level page

ny = htmlParse(getURLContent("https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html?action=click&module=Top%20Stories&pgtype=Homepage"))

# Then we find the HTML elements that have an immediate child text node that contains the string USA.json.
js = xpathSApply(ny, "//*[contains(./text(), 'USA.json')]", xmlValue)

#These are <script> elements containing JavaScript, not JSON.
# But we can find the URL with a regular expression in these
u = gsub('.*"(https://[^"]+USA.json)".*', "\\1", js)
u = unique(u)
# There is only 1 URL repeated multiple times.

# So now we have these

tt = getURLContent(u, verbose = TRUE, followlocation = TRUE)
library(RJSONIO)
us = fromJSON(tt)
length(us$data)

