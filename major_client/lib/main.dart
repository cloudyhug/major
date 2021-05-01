import 'package:flutter/material.dart';
import 'package:major_client/app_state.dart';
import 'package:major_client/http_service.dart';
import 'package:major_client/pages/home.dart';
import 'package:major_client/pages/info.dart';
import 'package:major_client/pages/register.dart';
import 'package:major_client/pages/results.dart';
import 'package:major_client/pages/voting.dart';

void main() {
  runApp(MyApp());
}

class MyApp extends StatelessWidget {
  // This widget is the root of your application.
  @override
  Widget build(BuildContext context) {
    AppState appState = AppState(
      httpService: HttpService()
    );
    return MaterialApp(
      title: 'Major',
      theme: ThemeData(
        primarySwatch: Colors.blueGrey
      ),
      initialRoute: '/',
      routes: {
        '/': (context) => HomePage(appState),
        '/register': (context) => RegisterPage(appState),
        '/voting': (context) => VotingPage(appState),
        '/results': (context) => ResultsPage(appState),
        '/info': (context) => InfoPage()
      }
    );
  }
}