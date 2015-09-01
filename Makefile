all:
	cd logos/full;\
	for file in *; do auto-scale "$$file" "../thumb/$${file%.*}.png" 60 40; done
