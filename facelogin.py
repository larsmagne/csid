#!/usr/bin/python3

# apt install python3-selenium

import time
import random
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.by import By
import json

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

# Login
driver.get("http://www.facebook.com")

time.sleep(5)

cookie_times = 0
while cookie():
    cookie_times += 1
    if cookie_times > 30:
        break;
    print("Waiting for cookie")
    time.sleep(1)

time.sleep(5)

#time.sleep(500)

driver.find_element(By.ID, "email").send_keys(user)
time.sleep(2)
driver.find_element(By.ID, "pass").send_keys(passwd)
time.sleep(3)
driver.find_element(By.NAME, "login").click()

time.sleep(120)

# Store cookies in a file
cookies = driver.get_cookies()
with open('facebook.json', 'w') as file:
    json.dump(cookies, file)

