FROM futtetennista/hakyll:4.9.8.0-ext

RUN apt-get-install --yes hunspell

RUN stack updgrade && \
    stack --resolver lts-9.14 install hlint

EXPOSE 8000

ENTRYPOINT ["bash"]
