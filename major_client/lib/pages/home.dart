import 'package:flutter/material.dart';
import 'package:major_client/api.dart';
import 'package:major_client/app_state.dart';
import 'package:major_client/util/either.dart';

class HomePage extends StatelessWidget {
  final AppState appState;

  HomePage(this.appState);

  @override
  Widget build(BuildContext context) {
    TextEditingController serverTextController = TextEditingController();
    TextEditingController portTextController = TextEditingController();

    return Scaffold(
      body: Center(
        child: FractionallySizedBox(
          heightFactor: 0.80,
          widthFactor: 0.60,
          child: Column(
            children: <Widget>[
              Text('major'),

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
                      appState.httpService.server = serverTextController.text;
                      appState.httpService.port = int.parse(portTextController.text);
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
                            Navigator.pushNamed(
                              context,
                              '/results',
                              arguments: state.info
                            );
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
      )
    );
  }

}