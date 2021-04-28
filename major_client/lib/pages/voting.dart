import 'package:flutter/material.dart';
import 'package:major_client/app_state.dart';
import 'package:major_client/api.dart';
import 'package:major_client/util/either.dart';
import 'package:major_client/util/unit.dart';

class VotingPage extends StatefulWidget {
  final AppState appState;

  VotingPage(this.appState);

  @override
  State<StatefulWidget> createState() => VotingPageState(appState);
}

class VotingPageState extends State<VotingPage> {
  final AppState appState;
  bool showPassword = false;
  String login = "";
  String password = "";
  TextEditingController loginTextController = TextEditingController();
  TextEditingController passwordTextController = TextEditingController();

  VotingPageState(this.appState);

  Widget build(BuildContext context) {
    final ElectionInfo info = ModalRoute.of(context)!.settings.arguments as ElectionInfo;

    appState.candidatesInfo = Map.fromIterable(
      info.candidatesInfo!,
      key: (candidateInfo) => (candidateInfo as CandidateInfo).id,
      value: (candidateInfo) => candidateInfo
    );

    List<Widget> candidateWidgets =
      info.candidatesInfo!.map((candidateInfo) {
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
          trailing: RatingDropdownButton(
            candidateInfo.id,
          )
        );
      }).toList();

    Widget loginWidget = Padding(
      padding: EdgeInsets.only(top: 8.0),
      child: TextField(
        keyboardType: TextInputType.text,
        controller: loginTextController,
        decoration: InputDecoration(
          labelText: 'Login',
          border: const OutlineInputBorder()
        ),
        onChanged: (text) => setState(() { login = text; })
      )
    );

    Widget passwordWidget = Padding(
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
            onPressed: () {
              setState(() { showPassword = !showPassword; });
            }
          )
        ),
        onChanged: (text) => setState(() { password = passwordTextController.text; })
      )
    );

    Widget voteWidget = Padding(
      padding: EdgeInsets.only(top: 8.0),
      child: SizedBox(
        width: double.infinity,
        child: ElevatedButton(
          child: const Text('Vote'),
          onPressed: appState.hasVoted ? null : () async {
            Authentication auth = Authentication(
              login: loginTextController.text,
              password: passwordTextController.text
            );
            Ballot ballot = Ballot(
              authentication: auth,
              shards: candidateWidgets.map((tile) {
                RatingDropdownButton dropdownButton =
                  (tile as ListTile).trailing! as RatingDropdownButton;
                return VoteShard(
                  candidateId: dropdownButton.candidateId,
                  grade: dropdownButton.dropdownValue.value
                );
              }).toList()
            );
            Either<String, Unit> res = await appState.httpService.vote(ballot);
            if (res.isRight()) {
              appState.hasVoted = true;
              Navigator.pop(context, res.right!);
            } else {
              ScaffoldMessenger.of(context).showSnackBar(
                SnackBar(content: Text(res.left!))
              );
            }
          }
        )
      )
    );

    List<Widget> widgets = [loginWidget, passwordWidget] + candidateWidgets + [voteWidget];

    return Scaffold(
      appBar: AppBar(
        title: const Text('Voting phase'),
        leading: IconButton(
          icon: Icon(Icons.arrow_back_rounded),
          onPressed: () { Navigator.pop(context); }
        )
      ),
      body: Center(
        child: Padding(
          padding: EdgeInsets.all(8.0),
          child: ListView(
            children: widgets
          )
        )
      )
    );
  }
}

class RatingDropdownButton extends StatefulWidget {
  final int candidateId;
  final Box<Rating> dropdownValue = Box(Rating.Terrible);

  RatingDropdownButton(this.candidateId);

  @override
  State<StatefulWidget> createState() => RatingDropdownButtonState();
}

class RatingDropdownButtonState extends State<RatingDropdownButton> {
  @override
  Widget build(BuildContext context) {
    return DropdownButton<Rating>(
      value: widget.dropdownValue.value,
      icon: const Icon(Icons.how_to_vote_rounded),
      items: possibleRatings.map((rating) => DropdownMenuItem<Rating>(
        value: rating,
        child: Text(showRating(rating))
      )).toList(),
      onChanged: (rating) => setState(() {
        widget.dropdownValue.value = rating!;
      })
    );
  }

}

class Box<T> {
  T value;
  Box(this.value);
}