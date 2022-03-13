# major

## Description

This is a simple client-server app to test the majority judgment voting system.

## Architecture

The project is comprised of a mobile client and a server. Many clients can connect to a same server, sending their votes through a very simple REST API.

I want to warn potential users about the fact that security properties are absolutely not taken into consideration. Using this tool, it is possible to rig the election and vote several times or abuse some API calls. The code does not scale either, for example because of the use of a memory database instead of something like PostgreSQL (there is a typeclass so that a real DB can be added easily though).

## Server usage

Use `stack build` in `major-server` to compile. Run the executable with `--help` to get more information.

## Client usage

Every voter opens the Android app, enters the IP and the port. He registers, votes, then waits for the results.

**NOTE**: The server does not handle TLS, and the Android application sends HTTPS requests. This means that both are not compatible as is. You will need to setup a reverse proxy on the server running the back-end, so that the content of the packets is not disclosed publicly **and** manual certificate management is not needed (with such web servers as Caddy).