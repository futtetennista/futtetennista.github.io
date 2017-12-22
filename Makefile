create-container:
	docker run -d -p 8000:8000 -v /Users/futtetennista/Developer/futtetennismo.io:/home/futtetennismo.io -w /home/futtetennismo.io --name futtetennismo.io futtetennista/hakyll:futtetennismo-0.1.1

rebuild-site:
	docker exec -d futtetennismo.io stack exec futtetennismo rebuild

watch-site:
	docker exec -d futtetennismo.io stack exec futtetennismo -- watch

build-image:
	ifndef FUTTETENNISMO_VERSION
	  $(error Please define a FUTTETENNISMO_VERSION)
	endif

	set +o pipefail

	docker load -i ./.caches/docker-futtetennismo.tar | true
	docker build --cache-from=futtetennista/hakyll-ext -t futtetennista/hakyll-ext:futtetennismo-$(FUTTETENNISMO_VERSION) .
	mkdir -p ./.caches
	docker save -o ./.caches/docker-futtetennismo.tar futtetennista/hakyll-ext
