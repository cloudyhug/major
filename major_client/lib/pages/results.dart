import 'package:flutter/material.dart';
import 'package:major_client/api.dart';
import 'package:major_client/app_state.dart';

class ResultsPage extends StatelessWidget {
  final AppState appState;

  ResultsPage(this.appState);

  @override
  Widget build(BuildContext context) {
    final ElectionInfo info = ModalRoute.of(context)!.settings.arguments as ElectionInfo;

    bool handlingWinner = true;
    Widget? crownIfWinner() {
      if (handlingWinner) {
        handlingWinner = false;
        return Icon(
          Icons.check_circle_outline_rounded,
          color: Colors.green
        );
      } else {
        return null;
      }
    }

    List<Widget> resultWidgets =
      <Widget>[Text('Participation: ${info.participation!.toStringAsFixed(2)}%')] +
      info.scores!.map((candidateScore) {
        CandidateInfo candidateInfo = appState.candidatesInfo[candidateScore.scoreId]!;
        int r = int.parse(candidateInfo.colour.substring(1, 3), radix: 16);
        int g = int.parse(candidateInfo.colour.substring(3, 5), radix: 16);
        int b = int.parse(candidateInfo.colour.substring(5), radix: 16);
        return ListTile(
          leading: Icon(
            Icons.circle,
            color: Color.fromARGB(255, r, g, b)
          ),
          title: Text(candidateInfo.name),
          subtitle: Text(candidateInfo.party),
          trailing: crownIfWinner()
        );
      }).toList();

    return Scaffold(
      appBar: AppBar(
        title: const Text('Results phase'),
        leading: IconButton(
          icon: Icon(Icons.arrow_back_rounded),
          onPressed: () { Navigator.pop(context); }
        )
      ),
      body: Center(
        child: ListView(
          children: resultWidgets
        )
      )
    );
  }
}