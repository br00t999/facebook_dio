# Facebook Dangerous Individuals and Organizations Snapshot

## References

* https://theintercept.com/2021/10/12/facebook-secret-blacklist-dangerous
* https://theintercept.com/document/2021/10/12/facebook-praise-support-and-representation-moderation-guidelines-reproduced-snapshot/
* https://theintercept.com/document/2021/10/12/facebook-praise-support-and-representation-moderation-guidelines-reproduced-snapshot/

## Overview

A little R project featuring code to extract and clean data from Facebook DIO snapshot found in 
The Intercept PDF dump

If you are not interested in actually parsing the pdf, you can simply use the CSV file
found in the data directory: 
`data/facebook-dangerous-individuals-and-organizations-list-reproduced-snapshot.csv`

## Parsing the PDF

If you want to parse the PDFs yourself set `parse_pdfs` and `rebuild_db` variables to `TRUE`
in the `facebook_dio_parser.R` script and then run the script. The parsed data will be dumped to 
csv in the data directory.

## Data Dictionary for CSV file

The following fields are found in the `data/facebook-dangerous-individuals-and-organizations-list-reproduced-snapshot.csv`
file:

* name: name of individual or organization
* category: category of individual or organization
* region: region of the world
* type: additional info on the individual or organization
* affiliated_with: individuals and organizations affilitated with this individual or organization
* designation_sources: source of designation
* page: page from the pdf file this entry comes from
* type_1: orgs or individuals
* type_2: crime, hate, militarized, terror or violent
* description: facebook provides a description of militarized social movements
