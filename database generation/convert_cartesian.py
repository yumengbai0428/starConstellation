import json
from math import cos, sin, radians

file_path='./parsed_data.json'
with open(file_path, 'r') as file:
    data = json.load(file)

cartesian_data={}
for constellation, stars in data.items():
    cartesian_data[constellation]={}
    for star, params in stars.items():
        a = radians((params['ra_hours']*15) + (params['ra_minutes']*0.25) + (params['ra_seconds']*0.004166))
        b = abs(params['dec_degrees']) + (params['dec_minutes']/60) + (params['dec_seconds']/3600)
        if params['dec_degrees']<0: 
            b=radians(b*-1)
        c = params['lr']

        x = (c*cos(b))*cos(a)
        y = (c*cos(b))*sin(a)
        z = c*sin(b)

        cartesian_data[constellation][star]=(x,y,z)

with open('cartesian.json', 'w') as file:
    json.dump(cartesian_data, file, indent=4)