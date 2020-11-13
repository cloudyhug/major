# major

## Description

This is a simple app to test the majority judgment voting system, which supposedly has better properties than the usual voting systems we encounter in our public elections.

## Architecture

The project is comprised of 2 executables, a client and a server. Many clients can connect to a same server, registering the votes through a dead simple REST API and doing the computations when we ask for it.

I want to warn potential users about the fact that security properties are absolutely not taken into consideration. Using this tool, it is indeed possible to cheat hard like a [democrat](https://media.giphy.com/media/U9CFWw4zEzlDO/giphy.gif) (gl hf USA).

**WIP: this project is not guaranteed to work as intended at the moment.**
