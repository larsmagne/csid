#!/usr/bin/python3

# apt install python3-selenium

import time
import random
import json
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.by import By

def cookie():
    for phrase in ['Tillat alle informasjonskapsler',
                   'Godta alle', 'Alle akzeptieren',
                   'Allow all cookies',
                   'Tillat nødvendige og valgfrie informasjonskapsler']:
        try:
            accept = driver.find_element(By.XPATH,
                                         "(//div[contains(@aria-label, '" + phrase + "')])[2]")
            print("Found button")
            time.sleep(5)
            accept.click()
            return False
        except Exception as e:
            print(e)
            pass
    return True

with open('facepass.txt') as f:
    user = f.readline().strip()
    passwd = f.readline().strip()
f.close()

with open('/tmp/faceurls.txt') as f:
    urls = f.readlines()
f.close()
    
# Open Crome
chrome_options = webdriver.ChromeOptions()
prefs = {"profile.default_content_setting_values.notifications" : 2}
chrome_options.add_experimental_option("prefs", prefs)
chrome_options.add_argument("--disable-notifications")
#chrome_options.add_argument("--headless")
chrome_options.add_argument("--disable-dev-shm-usage");
#chrome_options.add_argument('--no-sandbox')
chrome_options.add_experimental_option('prefs', {'intl.accept_languages': 'no'})
driver = webdriver.Chrome(options=chrome_options)

# Load cookies to a vaiable from a file
with open('facebook.json', 'r') as file:
    cookies = json.load(file)

driver.get("http://www.facebook.com")

# Set stored cookies to maintain the session
for cookie in cookies:
    driver.add_cookie(cookie)
    
# Login
time.sleep(6)
driver.get("http://www.facebook.com")
time.sleep(6)

# Fetch and dump all the URLs
for elem in urls:
    print(elem)
    bits = elem.split()
    times = 4
    # Fetch the URL.
    driver.get(bits[1])

    # Push "See More" some times.
    while times > 0:
        times -= 1
        driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
        print("Scrolled")
        time.sleep(random.randint(5, 20))
    html = driver.execute_script("return document.body.innerHTML;")
    with open("/tmp/face/face-" + bits[0] + ".html", "w") as f:
        f.write(html)
    print("Saved")
    time.sleep(random.randint(200, 400))

# Store cookies in a file
cookies = driver.get_cookies()
with open('facebook.json', 'w') as file:
    json.dump(cookies, file)

driver.quit()
