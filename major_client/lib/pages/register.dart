import 'package:flutter/material.dart';
import 'package:major_client/app_state.dart';
import 'package:major_client/api.dart';
import 'package:major_client/util/either.dart';
import 'package:major_client/util/unit.dart';

class RegisterPage extends StatefulWidget {
  final AppState appState;

  RegisterPage(this.appState);

  @override
  State<StatefulWidget> createState() => RegisterPageState(appState);
}

class RegisterPageState extends State<RegisterPage> {
  final AppState appState;
  bool showPassword = false;
  String login = "";
  String password = "";
  TextEditingController loginTextController = TextEditingController();
  TextEditingController passwordTextController = TextEditingController();

  RegisterPageState(this.appState);

  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Register phase'),
        leading: IconButton(
          icon: Icon(Icons.arrow_back_rounded),
          onPressed: () { Navigator.pop(context); }
        ),
      ),
      body: Center(
        child: Padding(
          padding: EdgeInsets.all(8.0),
          child: Column(
            children: <Widget>[
              Padding(
                padding: EdgeInsets.only(top: 8.0),
                child: TextField(
                  keyboardType: TextInputType.text,
                  controller: loginTextController,
                  decoration: InputDecoration(
                    labelText: 'Login',
                    border: const OutlineInputBorder()
                  ),
                  onChanged: (text) => setState(() { login = text; }),
                )
              ),

              Padding(
                padding: EdgeInsets.only(top: 8.0),
                child: TextField(
                  keyboardType: TextInputType.text,
                  obscureText: !showPassword,
                  controller: passwordTextController,
                  decoration: InputDecoration(
                    labelText: 'Password',
                    border: const OutlineInputBorder(),
                    suffixIcon: IconButton(
                      icon: Icon(
                        Icons.remove_red_eye,
                        color: showPassword ? Colors.blue : Colors.grey
                      ),
                      onPressed: () => setState(() { showPassword = !showPassword; })
                    )
                  ),
                  onChanged: (text) => setState(() { password = passwordTextController.text; })
                )
              ),

              Padding(
                padding: EdgeInsets.only(top: 8.0),
                child: SizedBox(
                  width: double.infinity,
                  child: ElevatedButton(
                    child: const Text('Register'),
                    onPressed: appState.hasRegistered ? null : () async {
                      Authentication auth = Authentication(
                        login: loginTextController.text,
                        password: passwordTextController.text
                      );
                      Either<String, Unit> res = await appState.httpService.register(auth);
                      if (res.isRight()) {
                        appState.hasRegistered = true;
                        Navigator.pop(context, res.right!);
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