"""
Web Scraping - Beautiful Soup
"""
# importing required libraries
import requests
from bs4 import BeautifulSoup
import pandas as pd

# target URL to scrape
url = "https://www.goibibo.com/hotels/hotels-in-shimla-ct/"
headers =  {
'User-Agent': "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/77.0.3865.90 Safari/537.36"
    }
# send request to download the data
response = requests.request("GET",url,headers=headers)
print(response)