import 'package:json_annotation/json_annotation.dart';

part 'api.g.dart';

enum Rating {
  Excellent, Good, Satisfactory, Sufficient, Disappointing, Bad, Terrible
}

String showRating(Rating r) {
  switch (r) {
    case Rating.Excellent: return 'Excellent';
    case Rating.Good: return 'Good';
    case Rating.Satisfactory: return 'Satisfactory';
    case Rating.Sufficient: return 'Sufficient';
    case Rating.Disappointing: return 'Disappointing';
    case Rating.Bad: return 'Bad';
    case Rating.Terrible: return 'Terrible';
  }
}

List<Rating> possibleRatings = [
  Rating.Excellent,
  Rating.Good,
  Rating.Satisfactory,
  Rating.Sufficient,
  Rating.Disappointing,
  Rating.Bad,
  Rating.Terrible
];

enum ElectionPhase {
  Register, Voting, Results
}

@JsonSerializable(createToJson: false)
class CandidateScore {
  final int scoreId;
  final Rating medianGrade;
  final double percentScore;
  CandidateScore({required this.scoreId, required this.medianGrade, required this.percentScore});
  factory CandidateScore.fromJson(Map<String, dynamic> json) => _$CandidateScoreFromJson(json);
}

@JsonSerializable(createToJson: false)
class ServerState {
  final ElectionPhase phase;
  final ElectionInfo? info;
  ServerState({required this.phase, this.info});
  factory ServerState.fromJson(Map<String, dynamic> json) => _$ServerStateFromJson(json);
}

@JsonSerializable(createToJson: false)
class ElectionInfo {
  final String tag;
  // when in Results phase
  final double? participation;
  final List<CandidateScore>? scores;
  // when in Voting phase
  final List<CandidateInfo>? candidatesInfo;
  ElectionInfo({required this.tag, this.participation, this.scores, this.candidatesInfo});
  factory ElectionInfo.fromJson(Map<String, dynamic> json) => _$ElectionInfoFromJson(json);
}

@JsonSerializable(createToJson: false)
class CandidateInfo {
  final int id;
  final String name;
  final String party;
  final String colour;
  CandidateInfo({required this.id, required this.name, required this.party, required this.colour});
  factory CandidateInfo.fromJson(Map<String, dynamic> json) => _$CandidateInfoFromJson(json);
}

@JsonSerializable(createFactory: false)
class VoteShard {
  final int candidateId;
  final Rating grade;
  VoteShard({required this.candidateId, required this.grade});
  Map<String, dynamic> toJson() => _$VoteShardToJson(this);
}

@JsonSerializable(createFactory: false)
class Ballot {
  final Authentication authentication;
  final List<VoteShard> shards;
  Ballot({required this.authentication, required this.shards});
  Map<String, dynamic> toJson() => _$BallotToJson(this);
}

@JsonSerializable(createFactory: false)
class Authentication {
  final String login;
  final String password;
  Authentication({required this.login, required this.password});
  Map<String, dynamic> toJson() => _$AuthenticationToJson(this);
}