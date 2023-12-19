import requests
from bs4 import BeautifulSoup
import time
import json

def fetch_page(url):
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3'
    }
    try:
        response = requests.get(url, headers=headers)
        if response.status_code == 200:
            return BeautifulSoup(response.content, 'html.parser')
    except requests.exceptions.ConnectionError:
        print("Connection error. Retrying...")
        time.sleep(5) 
        return fetch_page(url)
    except Exception as e:
        print(f"An error occurred: {e}")
    return None

def extract_stars(constellation_url):
    soup = fetch_page(constellation_url)
    if soup is None:
        return {}

    stars = {}
    table = soup.find('table', {'class': 'wikitable'})
    if table:
        rows = table.find_all('tr')
        header = rows[0]  # The header row
        headers = [th.get_text(strip=True) for th in header.find_all('th')]

        # Identify RA and Dec column indexes
        try:
            ra_index = headers.index('RA')
            dec_index = headers.index('Dec')
            lr_index = headers.index('Dist.(ly)')
        except ValueError:
            print(f"RA, Dec, or LR columns not found in {constellation_url}")
            return stars

        for row in rows[1:]: 
            cells = row.find_all('td')
            if len(cells) > max(ra_index, dec_index):
                star_name = cells[0].get_text(strip=True)
                right_ascension = cells[ra_index].get_text(strip=True)
                declination = cells[dec_index].get_text(strip=True)
                light_yrs = cells[lr_index].get_text(strip=True)
                if right_ascension and declination and light_yrs:
                    stars[star_name] = (right_ascension, declination, light_yrs)

    return stars


base_url = "https://en.wikipedia.org/wiki/Lists_of_stars_by_constellation"
main_page = fetch_page(base_url)

if main_page is None:
    print("Failed to fetch constellation list")
else:
    constellation_data = {}
    constellation_links = main_page.find_all('a', href=True)

    for link in constellation_links:
        title = link.get('title', '')
        if title.startswith('List of stars in'):
            constellation_name = title.split('List of stars in ')[-1]
            constellation_url = "https://en.wikipedia.org" + link['href']
            constellation_data[constellation_name] = extract_stars(constellation_url)

            print(f"Data extracted for {constellation_name}")
            time.sleep(1)  # wait for 1 second between requests
        if len(constellation_data)==88:
            break

    with open('constellation_data.json', 'w') as file:
        json.dump(constellation_data, file, indent=4)
    with open('constellation_data.txt', 'w') as file:
        file.write(str(constellation_data))

# if __name__ == "__main__":
#     main()
