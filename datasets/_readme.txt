Datasets
- fsbr: Frankfurter Straße, Bad Rothenfelde (https://goo.gl/maps/4DZyN25S4oZ9WQGA8)
- lsho: Lange Straße, Hessisch Oldendorf (https://goo.gl/maps/UHBhKsKojf7e9bUJ9)
- mke: Marktplatz, Königslutter am Elm* (https://goo.gl/maps/BuAUxr1RNXyX6F7XA)
	*here it contains only trajectories on street segment part of the layout

Parameters
- timestamp = date + time of recording
- id = road user id
- user = road user label [car, person]
- coordx = X coordinate in relation to the layout picture
- coordy = Y coordinate in relation to the layout picture
- speed_local = point speed (in relation to previous position)
- acc = point acceleration (in relation to previous position)
- zone = current position within the shared space [circulation, safe or activity zone]
- action = movement pattern, see below:
	*crossing: crossing through circulation zone/direct crossing
	*walking_safe: only using the safe/protected zones 
	*walking_circulation: extending/prolonging movement in the circulation zone
	*not_reliable_crossing: person is crossing, but the extracted position is not 100% reliable (too far away from the camera)
	*driving_through: driving through the shared space (on circulation zone)
	*parking: movement approaching parking spots
	*only_parking: no further "approaching" movement recorded (appeared on the video already parking)
	*driving_uturn: using the shared space to turn
	*driving_wrong: not respecting the design (i.e. driving in restrict area, wrong direction)
- design_element = certain element to which the respective road user is closer to in that position (i.e. parking spots, bench etc)
	*for that a buffer around the design element was created (i.e ca. 1m radius/around it)
	*"not_close" means that the the road user in that position was not in the "region" of a certain design element
- width_street = width of the street
- traffic = direction of traffic [one-way, two-way]

