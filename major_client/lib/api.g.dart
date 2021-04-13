// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'api.dart';

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

CandidateScore _$CandidateScoreFromJson(Map<String, dynamic> json) {
  return CandidateScore(
    scoreId: json['scoreId'] as int,
    medianGrade: _$enumDecode(_$RatingEnumMap, json['medianGrade']),
    percentScore: (json['percentScore'] as num).toDouble(),
  );
}

K _$enumDecode<K, V>(
  Map<K, V> enumValues,
  Object? source, {
  K? unknownValue,
}) {
  if (source == null) {
    throw ArgumentError(
      'A value must be provided. Supported values: '
      '${enumValues.values.join(', ')}',
    );
  }

  return enumValues.entries.singleWhere(
    (e) => e.value == source,
    orElse: () {
      if (unknownValue == null) {
        throw ArgumentError(
          '`$source` is not one of the supported values: '
          '${enumValues.values.join(', ')}',
        );
      }
      return MapEntry(unknownValue, enumValues.values.first);
    },
  ).key;
}

const _$RatingEnumMap = {
  Rating.Excellent: 'Excellent',
  Rating.Good: 'Good',
  Rating.Satisfactory: 'Satisfactory',
  Rating.Sufficient: 'Sufficient',
  Rating.Disappointing: 'Disappointing',
  Rating.Bad: 'Bad',
  Rating.Terrible: 'Terrible',
};

ServerState _$ServerStateFromJson(Map<String, dynamic> json) {
  return ServerState(
    phase: _$enumDecode(_$ElectionPhaseEnumMap, json['phase']),
    info: json['info'] == null
        ? null
        : ElectionInfo.fromJson(json['info'] as Map<String, dynamic>),
  );
}

const _$ElectionPhaseEnumMap = {
  ElectionPhase.Register: 'Register',
  ElectionPhase.Voting: 'Voting',
  ElectionPhase.Results: 'Results',
};

ElectionInfo _$ElectionInfoFromJson(Map<String, dynamic> json) {
  return ElectionInfo(
    tag: json['tag'] as String,
    participation: (json['participation'] as num?)?.toDouble(),
    scores: (json['scores'] as List<dynamic>?)
        ?.map((e) => CandidateScore.fromJson(e as Map<String, dynamic>))
        .toList(),
    candidatesInfo: (json['candidatesInfo'] as List<dynamic>?)
        ?.map((e) => CandidateInfo.fromJson(e as Map<String, dynamic>))
        .toList(),
  );
}

CandidateInfo _$CandidateInfoFromJson(Map<String, dynamic> json) {
  return CandidateInfo(
    id: json['id'] as int,
    name: json['name'] as String,
    party: json['party'] as String,
    colour: json['colour'] as String,
  );
}

Map<String, dynamic> _$VoteShardToJson(VoteShard instance) => <String, dynamic>{
      'candidateId': instance.candidateId,
      'grade': _$RatingEnumMap[instance.grade],
    };

Map<String, dynamic> _$BallotToJson(Ballot instance) => <String, dynamic>{
      'authentication': instance.authentication,
      'shards': instance.shards,
    };

Map<String, dynamic> _$AuthenticationToJson(Authentication instance) =>
    <String, dynamic>{
      'login': instance.login,
      'password': instance.password,
    };
