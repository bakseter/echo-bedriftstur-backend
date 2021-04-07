# Install dependencies
FROM haskell:latest as deps

WORKDIR /opt/build
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/

RUN apt-get update \
 && apt-get -yq --no-install-suggests --no-install-recommends install libpq-dev

RUN stack build --only-dependencies --copy-bins --install-ghc


# Build project using (cached) dependencies
FROM haskell:latest as build

WORKDIR /opt/build
COPY --from=deps /root/.stack /root/.stack/
COPY . /opt/build/

RUN apt-get update \
 && apt-get -yq --no-install-suggests --no-install-recommends install libpq-dev

RUN stack build --test --copy-bins


# Execute binary
FROM ubuntu:latest

WORKDIR /opt/app
COPY --from=build /root/.local/bin/echo-bedriftstur-backend-exe /opt/app
COPY . /opt/app

RUN apt-get update \
 && apt-get -yq --no-install-suggests --no-install-recommends install libpq-dev

ENTRYPOINT ["/opt/app/echo-bedriftstur-backend-exe"]
