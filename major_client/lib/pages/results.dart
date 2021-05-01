import 'package:flutter/material.dart';
import 'package:major_client/api.dart';
import 'package:major_client/app_state.dart';
import 'package:major_client/util/unit.dart';

class ResultsPage extends StatelessWidget {
  final AppState appState;

  const ResultsPage(this.appState);

  @override
  Widget build(BuildContext context) {
    final ElectionInfo info = ModalRoute.of(context)!.settings.arguments as ElectionInfo;
    if (appState.candidatesInfo.isEmpty) {
      appState.candidatesInfo = Map.fromIterable(
        info.candidatesInfo!,
        key: (candidateInfo) => (candidateInfo as CandidateInfo).id,
        value: (candidateInfo) => candidateInfo
      );
    }

    bool handlingWinner = true;
    Widget? crownIfWinner() {
      if (handlingWinner) {
        handlingWinner = false;
        return const Icon(
          Icons.check_circle_outline_rounded,
          color: Colors.green
        );
      } else {
        return null;
      }
    }

    Widget participationWidget = Padding(
      padding: const EdgeInsets.all(8.0),
      child: Center(
        child: Text('Participation: ${info.participation!.toStringAsFixed(2)}%')
      )
    );

    List<Widget> resultWidgets =
      [participationWidget] +
      info.scores!.map((candidateScore) {
        CandidateInfo candidateInfo = appState.candidatesInfo[candidateScore.scoreId]!;
        int r = int.parse(candidateInfo.colour.substring(1, 3), radix: 16);
        int g = int.parse(candidateInfo.colour.substring(3, 5), radix: 16);
        int b = int.parse(candidateInfo.colour.substring(5), radix: 16);
        String gradeStr = showRating(candidateScore.medianGrade);
        String scoreStr = candidateScore.percentScore.toStringAsFixed(2);
        return ListTile(
          leading: Icon(
            Icons.circle,
            color: Color.fromARGB(255, r, g, b)
          ),
          title: Text('${candidateInfo.name} : $gradeStr ($scoreStr%)'),
          subtitle: Text(candidateInfo.party),
          trailing: crownIfWinner()
        );
      }).toList();

    return Scaffold(
      appBar: AppBar(
        title: const Text('Results phase'),
        leading: IconButton(
          icon: const Icon(Icons.arrow_back_rounded),
          onPressed: () { Navigator.pop(context, Unit()); }
        )
      ),
      body: Center(
        child: Padding(
          padding: const EdgeInsets.all(8.0),
          child: ListView(
            children: resultWidgets
          )
        )
      )
    );
  }
}