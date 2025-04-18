# Sandboxes

This is a repo with my experimentations with different languages and frameworks.

the goal in fine is to have a fully functionning macro services suite, see issue #62 for more details

here are the contents:

## root

you'll find a docker compose file to launch all the services i need for the infra, for now a postgres/postgis, a rabbitMQ, a redis, a mongodb and its mongo-express.

it also links to the projects i have bundled as docker images, you will need to build them beforehand

run everything with a

```shell
docker compose up
# or
docker compose up -d
# or
docker compose up -d --build
```

everything is behind a nginx proxy so you need to update your /etc/hosts to

```
127.0.0.1 rust.sandboxes.local
127.0.0.1 haskell.sandboxes.local
127.0.0.1 nextjs.sandboxes.local
127.0.0.1 nestjs.sandboxes.local
127.0.0.1 angular.sandboxes.local
```

nginx will reroute calls to http://rust.sandboxes.local/health to the rust backend service, ...

## Rust

this is a backend web server built with the _functional core / imperative shell_ approach, watch [this conf](https://www.youtube.com/watch?v=P1vES9AgfC4) to know more.

this code uses
- diesel for binding to the db
- lapin for binding to rabbitMQ
- redis for binding to redis
- rocket for its REST web exposition

it also dont venture too far from rusts standard practices so `cargo` is your go to to work on it

```
$ cargo check/build/run/test/fmt
```

noteworthy: it handles auth through `Authorization: Bearer xxx` header with xxx being a jwt encoded with hmac, but there is no way for now to login via a endpoint, you need to generate a token yourself and use it in your http requests

## NextJs

this is a fullstack app built with nextjs.

it is loosely hexagonal, everything under /app are the driver adapters, under /infra are the driven adapters. the domain uses the _dependency parametrization_ pattern, /actions act as adherence layer between driver adapters and the domain logic while serving as dependency injector for the driven adapters.

this code uses
- typeorm for binding to the db
- next-auth for handling authentication, with google and github oauth providers configured
- mapbox for handling geojson data

it doesn't venture far from nextjs standards, so `npm run dev` and other `npm` scripts are your friends

note worthy: nextauth should allow to protect api endpoints but it does not seem to work for me right now, as a result `getServerSession()` is always `null` when calling api endpoints, even with the same cookies or headers. it'll be fixed at some point

also note worthy: i use typeorm here because prisma didn't have a built in way to handle postgis and geojson data types

## Haskell

this is just algorithmic sandbox to familiarize with haskell programming paradigm. It tries to model a schedule, for example a restaurant open tuesday through sunday from 11:30 to 3 and from 6 to 10:30, and if given a datetime this place is open

it uses cabal as launcher/package manager, so `cabal build`, `cabal run` and `cabal test` are your go to commands

## Angular

basic angular SPA, with SSR enabled.

This one is still fresh so there's not much to say.

It gets its data through the rust backend's REST api

Angular seems built with the _dependency injection_ pattern in mind so i'll use that for this part of the codebase

`npm start`, `npm run build`, `npm test` are your friends.

## Nestjs

very basic nestjs backend, it uses typeorm for binding to the postgres instance and mongoose for binding to mongodb. it exposes its stuff through a graphql endpoint available under /graphql, and a sandbox under /graphiql

Nest seems built with the _dependency injection_ pattern in mind so i'll use that for this part of the codebase.

Also the modules definition pattern is very nice when you want to use the _vertical slice_ pattern so i'll try using that at some point

once again `npm run` + scripts used by the nests community.

## Python

this is a barebone python sanic server with tortoise ORM for binding to the db.

I dont know what are the customs of the python community, so this part of the codebase has no local package manager with its dependency graph, no CI, none of it. just `$ python3 ./main.py` and fix errors you might encounter untill it works
