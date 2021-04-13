
import 'dart:convert';
import 'package:http/http.dart';
import 'package:major_client/api.dart';
import 'package:major_client/util/either.dart';
import 'package:major_client/util/unit.dart';

class HttpService {
  final String scheme = 'https';
  String server = "localhost";
  int port = 443;
  Client client = Client();

  Future<Either<String, ServerState>> getState() async {
    Uri uri = Uri(scheme: scheme, host: server, port: port, path: '/state');
    try {
      Response res = await client.get(uri);
      if (res.statusCode == 200) {
        return Either.right(ServerState.fromJson(jsonDecode(res.body)));
      } else {
        return Either.left('${res.statusCode} - ${res.headers['message']}');
      }
    } catch (e) {
      return Either.left(e.toString());
    }
  }

  Future<Either<String, Unit>> register(Authentication auth) async {
    Uri uri = Uri(scheme: scheme, host: server, port: port, path: '/register');
    try {
      Response res = await client.post(uri, body: jsonEncode(auth.toJson()));
      if (res.statusCode == 200) {
        return Either.right(Unit());
      } else {
        return Either.left('${res.statusCode} - ${res.headers['message']}');
      }
    } catch (e) {
      return Either.left(e.toString());
    }
  }

  Future<Either<String, Unit>> vote(Ballot ballot) async {
    Uri uri = Uri(scheme: scheme, host: server, port: port, path: '/vote');
    try {
      Response res = await client.post(uri, body: jsonEncode(ballot.toJson()));
      if (res.statusCode == 200) {
        return Either.right(Unit());
      } else {
        return Either.left('${res.statusCode} - ${res.headers['message']}');
      }
    } catch (e) {
      return Either.left(e.toString());
    }
  }
}