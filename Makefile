.PHONY: rebuild-site watch-site build-image

.EXPORT_ALL_VARIABLES:
image = futtetennista/hakyll-ext:futtetennismo-0.1.1

rebuild-site:
	docker exec -d futtetennismo stack exec futtetennismo -- rebuild

watch-site:
	# docker exec -d futtetennismo stack exec futtetennismo -- watch
	docker run -d --rm -p 8000:8000 \
    -v /Users/futtetennista/Developer/futtetennismo.io:/home/futtetennismo.io \
    -w /home/futtetennismo.io \
    --name futtetennismo  \
    --entrypoint stack \
    $$image exec futtetennismo -- watch  # -s stack exec futtetennismo -- watch

build-image:
	set +o pipefail
	docker build -t $$image .
