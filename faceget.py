#!/usr/bin/python3

from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait

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
chrome_options.add_experimental_option("prefs",prefs)
driver = webdriver.Chrome(options=chrome_options)

# Login
driver.get("http://www.facebook.com")
accept = driver.find_element_by_xpath("//button[@title='Godta alle']")
accept.click()

driver.find_element_by_id("email").send_keys(user)
driver.find_element_by_id("pass").send_keys(passwd)
driver.find_element_by_name("login").click()

driver.get("http://www.facebook.com")

# Fetch and dump all the URLs
for elem in urls:
    print(elem)
    bits = elem.split()
    driver.get(bits[1])
    html = driver.execute_script("return document.body.innerHTML;")
    with open("/tmp/face-" + bits[0] + ".html", "w") as f:
        f.write(html)

driver.quit()
