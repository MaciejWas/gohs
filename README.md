# gohs
Do you like Haskell? Do you like baduk? If yes, you might like this repo.

* This will be a event-driven game server. 
* Might be a good tutorial on how to build microservices with Haskell.
  
# How to run
Provided Haskell (ghcup, stack) and Redis is installed:

```
$ redis-server&
$ cd {repository root}/microservices/gate
$ stack run&
$ cd {repository root}/microservices/auth
$ stack run&
```
