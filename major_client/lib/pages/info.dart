import 'package:flutter/material.dart';

class InfoPage extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Information'),
        leading: IconButton(
          icon: const Icon(Icons.arrow_back_rounded),
          onPressed: () { Navigator.pop(context); }
        ),
      ),
      body: ListView.builder(
        itemBuilder: (context, index) => EntryItem(data[index]),
        itemCount: data.length
      )
    );
  }
}

class Entry {
  final String title;
  final List<Entry> children;
  const Entry(this.title, [this.children = const <Entry>[]]);
}

class EntryItem extends StatelessWidget {
  final Entry entry;

  const EntryItem(this.entry);

  Widget buildTiles(Entry root) {
    if (root.children.isEmpty)
      return ListTile(title: Text(root.title));
    else
      return ExpansionTile(
        key: PageStorageKey<Entry>(root),
        title: Text(root.title),
        children: root.children.map(buildTiles).toList()
      );
  }

  @override
  Widget build(BuildContext context) {
    return buildTiles(entry);
  }
}

const List<Entry> data = <Entry>[
  Entry(
    'How does the app work?',
    <Entry>[
      Entry('The election happens in 3 phases.'),
      Entry(
        'The first phase is registration. Every user chooses a login and a password, so that ' +
        'during the vote, no one can steal his identity.'
      ),
      Entry(
        'The second phase is voting. Every user willing to vote enters his login and password, ' +
        'and gives a grade to every candidate.'
      ),
      Entry(
        'The third and last phase is seeing the results. The scores are computed and the ' +
        'candidates are sorted. The one at the top is the winner.'
      )
    ]
  ),
  Entry(
    'How does the theoretical voting system work?',
    <Entry>[
      Entry(
        'This app is a client for an implementation of the majority judgment election system. ' +
        'The main difference between this system and more traditional election systems that are ' +
        'currently in effect in most democratic countries is that instead of choosing one ' +
        'candidate over the others, the voter gives grades to every candidate in the election. '
      ),
      Entry(
        'The grades range from "Terrible" to "Excellent". At the end of the voting phase, for ' +
        'each candidate, the system computes the majority judgment, which is the median grade. ' +
        'A candidate is given the majority judgment M if at least half of the voters chose at ' +
        'least this grade for him. The winner is the candidate with the best majority judgment. ' +
        'In case of a draw, percentages are compared.'
      ),
      Entry(
        'If two candidates A and B are given the same majority judgment, say "Good", with 68.7% ' +
        'for candidate A and 69.4% for candidate B, it means that 68.7% of the voters chose at ' +
        'least "Good" for A, and 69.4% of the voters chose at least "Good" for B. In this case, ' +
        'there is a draw in the median grades, but the numbers prove that B is the winner. ' +
        'Indeed, candidate B got 0.7% more voters choosing "Good" or better for him.'
      ),
    ]
  )
];