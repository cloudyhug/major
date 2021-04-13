import 'package:major_client/api.dart';
import 'package:major_client/http_service.dart';

class AppState {
  final HttpService httpService;
  bool hasRegistered = false;
  bool hasVoted = false;
  Map<int, CandidateInfo> candidatesInfo = Map();

  AppState({required this.httpService});
}