FROM futtetennista/hakyll:4.8.0.0-ext

RUN stack --resolver lts-9.14 install hlint

EXPOSE 8000

ENTRYPOINT ["bash"]
