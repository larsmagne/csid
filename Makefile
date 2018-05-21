deploy:
	rsync -av logos assets csid.css csid.js \
	cross.png home-cross.png summaries \
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
