import 'package:flutter/material.dart';
import 'package:major_client/app_state.dart';
import 'package:major_client/http_service.dart';
import 'pages/home.dart';
import 'pages/register.dart';
import 'pages/voting.dart';
import 'pages/results.dart';

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
      title: 'Flutter Demo',
      theme: ThemeData(
        primarySwatch: Colors.blueGrey
      ),
      initialRoute: '/',
      routes: {
        '/': (context) => HomePage(appState),
        '/register': (context) => RegisterPage(appState),
        '/voting': (context) => VotingPage(appState),
        '/results': (context) => ResultsPage(appState)
      }
    );
  }
}