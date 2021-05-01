import 'package:flutter/material.dart';
import 'package:http/http.dart';
import 'package:major_client/api.dart';
import 'package:major_client/app_state.dart';
import 'package:major_client/util/either.dart';

class HomePage extends StatelessWidget {
  final AppState appState;

  const HomePage(this.appState);

  @override
  Widget build(BuildContext context) {
    TextEditingController serverTextController = TextEditingController();
    TextEditingController portTextController = TextEditingController();

    return Scaffold(
      body: Center(
        child: Padding(
          padding: EdgeInsets.all(8.0),
          child: Column(
            children: <Widget>[
              Padding(
                padding: EdgeInsets.only(top: 24.0),
                child: Image.asset(
                  'assets/logo.png',
                  width: 300.0,
                  height: 300.0
                )
              ),

              Padding(
                padding: EdgeInsets.only(top: 8.0),
                child: TextField(
                  keyboardType: TextInputType.url,
                  controller: serverTextController,
                  decoration: InputDecoration(
                    labelText: 'Server',
                    hintText: 'IP address or domain name',
                    border: const OutlineInputBorder()
                  )
                )
              ),

              Padding(
                padding: EdgeInsets.only(top: 8.0),
                child: TextField(
                  keyboardType: TextInputType.number,
                  controller: portTextController,
                  decoration: InputDecoration(
                    labelText: 'Port',
                    border: const OutlineInputBorder()
                  )
                )
              ),

              Padding(
                padding: EdgeInsets.only(top: 8.0),
                child: SizedBox(
                  width: double.infinity,
                  child: ElevatedButton(
                    child: const Text('Connect to server'),
                    onPressed: () async {
                      var server = serverTextController.text;
                      var port = int.parse(portTextController.text);
                      if (server != appState.httpService.server
                          || port != appState.httpService.port) {
                        appState.httpService.server = serverTextController.text;
                        appState.httpService.port = int.parse(portTextController.text);
                        appState.httpService.client = Client();
                        appState.candidatesInfo = Map();
                        appState.hasRegistered = false;
                        appState.hasVoted = false;
                      }
                      Either<String, ServerState> res = await appState.httpService.getState();
                      if (res.isRight()) {
                        ServerState state = res.right!;
                        switch (state.phase) {
                          case ElectionPhase.Register:
                            var navRes = await Navigator.pushNamed(context, '/register');
                            if (navRes != null) {
                              ScaffoldMessenger.of(context).showSnackBar(
                                SnackBar(content: const Text('Registered successfully'))
                              );
                            }
                            break;
                          case ElectionPhase.Voting:
                            var navRes = await Navigator.pushNamed(
                              context,
                              '/voting',
                              arguments: state.info
                            );
                            if (navRes != null) {
                              ScaffoldMessenger.of(context).showSnackBar(
                                SnackBar(content: const Text('Voted successfully'))
                              );
                            }
                            break;
                          case ElectionPhase.Results:
                            Navigator.pushNamed(context, '/results', arguments: state.info);
                        }
                      } else {
                        ScaffoldMessenger.of(context).showSnackBar(
                          SnackBar(content: Text('Error: ${res.left!}'))
                        );
                      }
                    },
                  )
                )
              )
            ]
          )
        )
      ),
      floatingActionButton: FloatingActionButton(
        child: const Icon(Icons.info_outline_rounded),
        onPressed: () {
          Navigator.pushNamed(context, '/info');
        }
      )
    );
  }

}