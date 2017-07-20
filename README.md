# Query Accessibility

*R*   

####AUTHORS:

The Open Source Routing Machine (OSRM) is a tool that calculates optimal routes for a given geographical area and a given transport profile (car, bicycle, pedestrian, etc). How to use OSRM depends on the operating system you are using: Mac and Linux are supported by the OSRM team, while we have provided code for setting up OSRM on Windows.
## Mac and Linux
Up-to-date instructions for installing OSRM on Mac and Linux are given on the OSRM project page:https://github.com/Project-OSRM/osrm-backend#quick-start .  Once running, the url of the OSRM server can be passed to the provided R code using the `osrm.url` variable.
## Windows
1. Download an OpenStreetMap `.osm.pbf` file for the region you are interested in.  You can get these from sources such as http://download.geofabrik.de/ . The location of this file should be the `osm.pbf.path` variable in the R code.
2. Set up a transport `.lua` profile. It's best to start with one of the profiles included in the `lib/osrm/profiles`, and modify it to your needs. You can change things like speeds for different road surfaces, penalties for turns, and which road classes are allowed to be taken. The location of this file should be the `osrm.profile.path` variable.
3. Modify the other parameters of the code as required/desired and run. The function `StartOSRMServer` will set up an OSRM server that is used by the rest of the code, or could be accessed for your own calculations.
