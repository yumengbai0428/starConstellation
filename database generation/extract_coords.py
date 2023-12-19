import json
import re

def parse_ra_dec(ra, dec, star_name, constellation_name):
    try:
        ra_match = re.match(r'(\d+)h(\d+)m([\d.]+)s', ra)
        if not ra_match:
            raise ValueError(f"RA format error in star {star_name}: {ra}")
        ra_hours, ra_minutes, ra_seconds = ra_match.groups()

        dec_match = re.match(r'([+-]?\d+|\u2212\d+)\u00b0\u00a0(\d+)\u2032\u00a0([\d.]+)\u2033', dec)
        if not dec_match:
            raise ValueError(f"Dec format error in star {star_name}: {dec}")
        dec_degrees, dec_minutes, dec_seconds = dec_match.groups()
        dec_degrees = dec_degrees.replace('\u2212', '-')

        return {
            'ra_hours': float(ra_hours), 'ra_minutes': float(ra_minutes), 'ra_seconds': float(ra_seconds),
            'dec_degrees': float(dec_degrees), 'dec_minutes': float(dec_minutes), 'dec_seconds': float(dec_seconds)
        }
    except Exception as e:
        print(f"\tError parsing star {star_name} in {constellation_name}: {e}")
        return None


def parse_json_file(file_path):
    with open(file_path, 'r') as file:
        data = json.load(file)

    parsed_data = {}
    for constellation, stars in data.items():
        parsed_data[constellation] = {}
        for star, coords in stars.items():
            ra, dec, lr = coords 
            parsed_coords = parse_ra_dec(ra, dec, star, constellation)
            if parsed_coords:
                try:
                    parsed_coords['lr']=float(lr)
                    parsed_data[constellation][star] = parsed_coords
                except:
                    print(f"\tConversion error for light year dist. {lr} in {constellation}, star {star}")
                    print(f"\tInvalid data for star {star} in {constellation}: SKIP")
                    continue
            else:
                print(f"\tInvalid data for star {star} in {constellation}: SKIP")
        print(f"Finished parsing: {constellation}")

    return parsed_data


# path to json file
file_path = './constellation_data.json'
parsed_data = parse_json_file(file_path)

with open('parsed_data.json', 'w') as file:
    json.dump(parsed_data, file, indent=4)