# Brutti lovers :smiling_face_with_three_hearts:
---
Stay angry, stay Brutti

## Final project

I found this to extract data of google fit (I haven't tested it)
- https://gist.github.com/danielperna84/a02c307e123036973845e85b326cc940
- https://developers.google.com/fit/rest/v1/using-sessions

Information on data types:
- https://developers.google.com/fit/datatypes (Public data types)

TODO (Pre-Processing):

 - Ordering the sessions from Google
 - How to represent 1 minute from the Arduino data to merge with google data (think about both options)
 - How should we summarize data
 - Look how to get the altitude
 - Continue work on the merging (over time series sessions)

Data Collection Pipeline:
 - Start first the Arduino App, then Google Fit
 - Stop Google fit first, then stop Arduino
 - Sensors:
	- All in case of doubt
