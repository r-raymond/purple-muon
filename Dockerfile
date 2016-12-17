FROM alpine
RUN apk update

# install basic dev tools, ghc, gmp & zlib
RUN echo "https://s3-us-west-2.amazonaws.com/alpine-ghc/8.0" >> /etc/apk/repositories
ADD https://raw.githubusercontent.com/mitchty/alpine-ghc/master/mitch.tishmack%40gmail.com-55881c97.rsa.pub \
    /etc/apk/keys/mitch.tishmack@gmail.com-55881c97.rsa.pub
RUN apk update
RUN apk add alpine-sdk git ca-certificates ghc gmp-dev zlib-dev

# grab a recent binary of stack
ADD https://s3.amazonaws.com/static-stack/stack-1.1.2-x86_64 /usr/local/bin/stack
RUN chmod 755 /usr/local/bin/stack

# link in project
ADD ./ /usr/src/purple
WORKDIR /usr/src/purple

# build
RUN stack --local-bin-path /usr/src/purple/bin install
RUN ls -alh bin
RUN ldd bin/purple-muon-server
RUN ldd bin/purple-muon-client
RUN du -hs bin/purple-muon-server
RUN du -hs bin/purple-muon-client
