FROM futtetennista/hakyll:4.9.8.0-ext

# apt is updated in the base image
RUN apt-get install --yes \
    hunspell \
    xz-utils

RUN stack upgrade && \
    stack --resolver lts-9.14 install hlint

# RUN stack build futtetennismo:exe:futtetennismo

EXPOSE 8000

ENTRYPOINT ["bash"]
