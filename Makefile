create-container:
	docker run -it -d -p 8000:8000 -v $(shell pwd):/home/futtetennismo.io -w /home/futtetennismo.io --name=futtetennismo.io futtetennista/hakyll
