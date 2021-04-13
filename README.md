# major

## Description

This is a simple client-server app to test the majority judgment voting system, which supposedly has better properties than the usual voting systems we encounter in our public elections.

## Architecture

The project is comprised of a Flutter client and a Haskell server. Many clients can connect to a same server, registering the votes through a dead simple REST API and doing the computations. There used to be a Haskell CLI client but an Android app is more convenient so it is dead code at the moment, even though I do not want to delete it (it was a nice occasion to learn Brick).

I want to warn potential users about the fact that security properties are absolutely not taken into consideration. Using this tool, it is possible to rig the election and vote several times or abuse some API calls. The code does not scale either, for example because of the use of simple standard library data structures instead of efficient, distributed, production-ready ones, but this repository is a kind of proof of concept so it is not a real issue. However, if the admin is faithful, with the code only he will be able to access neither passwords (they are stored as hashes) nor vote contents (processed on arrival and request deleted afterwards).

## Server usage

Use `stack build` in `major_server` to compile. The produced executable is meant to be used like this:

- `./major-server [port] [config_file] [admin_login] [admin_password]`

For example, if we were to simulate the 2017 French presidential election in a majority judgment fashion, here is how we would do. Let the national vote IP and port be 200.120.5.5 and 8888. The server would be launched like this:
```
./major-server 8888 example_config.json my_admin_login my_admin_password
```

Every voter would enter the IP and the port in the Android application.

Then everyone would first register (once!) then vote and wait for the results. The administrator can make the state machine go forward, from state `Register` to `Voting` and then `Results`. In the `Results` state, everyone can see the results of the election and the winner.