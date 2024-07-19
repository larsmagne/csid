dist: images dark-images deploy

deploy:
	rsync -av logos dark-logos \
	assets csid.css dark.css csid.js sha1.js \
	pikaday.js \
	*.png splash-*.jpg jquery* help.html \
	www@quimby:html/circus/csid/

images:
	if [ ! -d logos/thumb ]; then mkdir logos/thumb; fi;\
	if [ ! -d logos/larger ]; then mkdir logos/larger; fi;\
	cd logos/full;\
	for file in *; do\
		auto-scale "$$file" "../thumb/$${file%.*}.png" 60 40;\
		auto-scale "$$file" "../larger/$${file%.*}.png" 300 100;\
		auto-scale "$$file" "../thumb/$${file%.*}x2.png" 120 80;\
		auto-scale "$$file" "../larger/$${file%.*}x2.png" 600 200;\
	done

dark-images:
	if [ ! -d dark-logos/thumb ]; then mkdir -p dark-logos/thumb; fi; \
	if [ ! -d dark-logos/larger ]; then mkdir -p dark-logos/larger; fi; \
	for img in logos/thumb/*.png logos/larger/*.png logos/thumb/*.webp logos/larger/*.webp; do \
		if [ ! -e "dark-$$img" ]; then \
			convert "$$img" -bordercolor none -border 10x10 -background white -alpha background -channel A -blur 0x10 -level 0,70% "dark-$$img"; \
               fi ; \
	done
