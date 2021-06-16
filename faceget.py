#!/usr/bin/python3

import time
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import NoSuchElementException

def cookie():
    for phrase in ['Godta alle', 'Alle akzeptieren']:
        try:
            accept = driver.find_element_by_xpath("//button[@title='" + phrase + "']")
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
chrome_options.add_argument("--headless")
driver = webdriver.Chrome(options=chrome_options)

# Login
driver.get("http://www.facebook.com")

while cookie():
    print("Waiting for cookie")
    time.sleep(1)

driver.find_element_by_id("email").send_keys(user)
driver.find_element_by_id("pass").send_keys(passwd)
driver.find_element_by_name("login").click()

driver.get("http://www.facebook.com")

# Fetch and dump all the URLs
for elem in urls:
    print(elem)
    bits = elem.split()
    times = 5
    max = 20
    # Fetch the URL.
    driver.get(bits[1])
    # Push "See More" some times.
    while times > 0:
        times -= 1
        for phrase in ['See More', 'Se flere']:
            try:
                path = "//*[text()='" + phrase + "']"
                more = driver.find_element_by_xpath(path)
                print("Got the more")
                more.click()
                time.sleep(2)
                # If there's two of these, keep trying until the first
                # goes away.
                if len(driver.find_elements_by_xpath(path)) > 1:
                    times = 1
            except NoSuchElementException:
                pass
        max -= 1
        if max < 0:
            times = 0
    html = driver.execute_script("return document.body.innerHTML;")
    with open("/tmp/face-" + bits[0] + ".html", "w") as f:
        f.write(html)

driver.quit()
