#!/usr/bin/python3

# apt install python3-selenium

import time
import random
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.by import By

def cookie():
    for phrase in ['Godta alle', 'Alle akzeptieren',
                   'Tillat alle informasjonskapsler',
                   'Tillat nødvendige og valgfrie informasjonskapsler']:
        try:
            accept = driver.find_element(By.XPATH,
                                         "//span[text()='" + phrase + "']")
            time.sleep(5)
            accept.click()
            return False
        except Exception:
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
time.sleep(7)

# Reload the main page -- it seems to like this.
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

driver.quit()
