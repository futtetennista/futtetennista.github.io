create-container:
	docker run -d -p 8000:8000 -v /Users/futtetennista/Developer/futtetennismo.io:/home/futtetennismo.io -w /home/futtetennismo.io --name=futtetennismo.io futtetennista/hakyll:futtetennismo-0.1.1

rebuild-site:
	docker exec -d futtetennismo.io stack exec futtetennismo rebuild

watch-site:
	docker exec -d futtetennismo.io stack exec futtetennismo -- watch
