# major

## Description

This is a simple client-server app to test the majority judgment voting system, which supposedly has better properties than the usual voting systems we encounter in our public elections.

## Architecture

The project is comprised of 2 executables, a client and a server. Many clients can connect to a same server, registering the votes through a dead simple REST API and doing the computations when we ask for it.

I want to warn potential users about the fact that security properties are absolutely not taken into consideration. Using this tool, it is possible to rig the election and vote several times or abuse some API calls. The code does not scale either, for example because of the use of linked lists instead of efficient and distributed data structures, but this repository is a kind of proof of concept so it is not a real issue.

## Usage

Use `stack build` to compile. The two produced executables are meant to be used like this:

- `./major-server [port] [candidates ...]`
- `./major-client [ip] [port]`

For example, if we were to simulate the 2017 French presidential election in a majority judgment fashion (and assume there is no fraud), here is how we would do. Let the national vote IP and port be 200.120.5.5 and 8888. The server would be launched like this:
```
./major-server 8888 "Jean Lassalle" "François Asselineau" "François Fillon" "Jacques Cheminade" \
                    "Jean-Luc Mélenchon" "Emmanuel Macron" "Philippe Poutou" "Nathalie Arthaud" \
                    "Marine Le Pen" "Benoît Hamon" "Nicolas Dupont-Aignan"
```
And here is what every voter would type in his terminal:
```
./major-client 200.120.5.5 8888
```
Then everyone would rank the candidates and then type `/vote`. Note that a temporary result can be obtained with `/result`, and a PNG with the visual results (made with gnuplot) can be generated on the server side through `/generate` (here is why it is unsafe).
