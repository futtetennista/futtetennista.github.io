FROM futtetennista/hakyll-ext:4.12.5.1

# apt is updated in the base image
RUN apt-get update && apt-get install --yes \
  hunspell \
  xz-utils

RUN stack upgrade && \
  stack --resolver lts-13.7 install --fast \
    hlint

COPY    . /home/futtetennismo.io
WORKDIR /home/futtetennismo.io
RUN     stack install --fast futtetennismo

EXPOSE 8000

ENTRYPOINT ["stack", "exec", "futtetennismo"]
CMD ["-- watch"]

# CMD stack install --fast futtetennismo
# CMD stack exec futtetennismo -- watch