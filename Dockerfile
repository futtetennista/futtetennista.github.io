ARG GHC_VERSION=8.0.2

FROM haskell:$GHC_VERSION

ARG SITE_NAME

# https://unix.stackexchange.com/questions/87745/what-does-lc-all-c-do
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

RUN stack install hakyll && \
    hakyll-init $SITE_NAME && \
    cd $SITE_NAME && \
    stack init && \
    stack build && \
    stack exec site build

EXPOSE 8000

WORKDIR home/$SITE_NAME

# --host is important: hakyll uses 127.0.0.1 by default but that
# does not play well with docker
# ENTRYPOINT ["stack", "exec", "site", "watch"] #, "-- --host=0.0.0.0"]
ENTRYPOINT ["bash"]
