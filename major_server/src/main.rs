use std::env;
use std::collections::{HashMap, HashSet};
use actix_web::{
  App, HttpResponse, http::{StatusCode, HeaderName, HeaderValue}, HttpServer,
  Responder, web, error
};
use actix_cors::Cors;
use serde_json;
use std::fs;
use std::sync::Mutex;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;

mod api;
mod matrix;
mod major;

use api::{ElectionPhase, ElectionInfo, CandidateInfo, CandidateScore, Authentication, Ballot,
          ServerState};
use matrix::Matrix;

fn hash(password: &String) -> u64 {
  let mut hasher = DefaultHasher::new();
  hasher.write(password.as_bytes());
  hasher.finish()
}

fn is_valid_config(config: &Vec<CandidateInfo>) -> bool {
  true // todo
}

struct AppState {
  candidates_info: Vec<CandidateInfo>,
  admin_login: String,
  admin_pw_hash: u64,
  election_phase: Mutex<ElectionPhase>,
  number_votes_cast: Mutex<u32>,
  user_auth: Mutex<HashMap<String, u64>>,
  user_votes: Mutex<HashSet<String>>,
  vote_data: Mutex<Matrix<u32>>,
  vote_results: Mutex<Vec<CandidateScore>>
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
  let args: Vec<String> = env::args().collect();
  if args.len() != 5 {
    panic!("Wrong number of arguments");
  }
  let port_str = &args[1];
  let input_file = &args[2];
  let admin_login = &args[3];
  let admin_password = &args[4];
  let admin_pw_hash = hash(&admin_password);
  let port = port_str.parse().unwrap_or(0);
  if port < 1024 {
    panic!("Invalid port");
  }

  // app state
  let candidates_info = {
    let data = fs::read_to_string(&input_file).expect("Config file not found");
    serde_json::from_str(&data).expect("Bad config format")
  };
  if !is_valid_config(&candidates_info) {
    panic!("Invalid config content");
  }
  let user_auth = HashMap::new();
  let user_votes = HashSet::new();
  
  let election_phase = ElectionPhase::Register;
  let number_votes_cast = 0;
  // cell (i, k) is the number of ratings k for candidate i
  let vote_data = Matrix::new(candidates_info.len(), 7, 0);

  let app_state = web::Data::new(AppState {
    candidates_info: candidates_info,
    admin_login: admin_login.clone(),
    admin_pw_hash: admin_pw_hash,
    election_phase: Mutex::new(election_phase),
    number_votes_cast: Mutex::new(number_votes_cast),
    user_auth: Mutex::new(user_auth),
    user_votes: Mutex::new(user_votes),
    vote_data: Mutex::new(vote_data),
    vote_results: Mutex::new(vec!())
  });

  HttpServer::new(move || App::new()
    .wrap(Cors::permissive())
    .app_data(
      web::JsonConfig::default().error_handler(|err, _req| {
        let e = format!("{:?}", err);
        error::InternalError::from_response(err, HttpResponse::Conflict().body(e))
            .into()
      })
    )
    .app_data(app_state.clone())
    .service(web::resource("/state").route(web::get().to(state)))
    .service(web::resource("/register").route(web::post().to(register)))
    .service(web::resource("/vote").route(web::post().to(vote)))
    .service(web::resource("/forward").route(web::put().to(forward)))
    .service(web::resource("/users").route(web::post().to(users))))
    .bind(("127.0.0.1", port as u16))?
    .run()
    .await
}

// input body: empty
// output body: ServerState (JSON)
async fn state(data: web::Data<AppState>) -> impl Responder {
  let state =
    match *data.election_phase.lock().unwrap() {
      ElectionPhase::Register => ServerState::new(ElectionPhase::Register, None),
      ElectionPhase::Voting =>
        ServerState::new(
          ElectionPhase::Voting,
          Some(ElectionInfo {
            participation: None,
            scores: None,
            candidatesInfo: data.candidates_info.clone()
          })
        ),
      ElectionPhase::Results => {
        let number_votes_cast = *data.number_votes_cast.lock().unwrap();
        if number_votes_cast == 0 {
          ServerState::new(ElectionPhase::Results, None)
        } else {
          let number_users_registered = (*data.user_auth.lock().unwrap()).len();
          let participation_rate = (number_votes_cast as f64) / (number_users_registered as f64);
          ServerState::new(
            ElectionPhase::Results,
            Some(ElectionInfo {
              participation: Some(participation_rate * 100.0),
              scores: Some((*data.vote_results.lock().unwrap()).clone()),
              candidatesInfo: data.candidates_info.clone()
            })
          )
        }
      }
    };
  println!("[info] /state");
  return HttpResponse::Ok()
    .content_type("application/json; charset=utf-8")
    .json(state)
}

// input body: Authentication (JSON)
// output body: empty
async fn register(
  body: web::Json<Authentication>,
  data: web::Data<AppState>
) -> impl Responder {
  println!("[info] /register");
  if *data.election_phase.lock().unwrap() != ElectionPhase::Register {
    let mut res = HttpResponse::new(StatusCode::BAD_REQUEST);
    res.headers_mut().insert(
      HeaderName::from_static("message"),
      HeaderValue::from_static("Election is not in Register phase")
    );
    println!("wrong phase");
    return res;
  } else {
    let user_auth = &mut *data.user_auth.lock().unwrap();
    if user_auth.get(&body.login).is_some() {
      let mut res = HttpResponse::new(StatusCode::CONFLICT);
      res.headers_mut().insert(
        HeaderName::from_static("message"),
        HeaderValue::from_static("User already registered")
      );
      println!("double registration");
      return res; 
    } else {
      let auth = body.into_inner();
      println!("{} registered", auth.login.clone());
      user_auth.insert(auth.login, hash(&auth.password));
      let mut res = HttpResponse::new(StatusCode::OK);
      res.headers_mut().insert(
        HeaderName::from_static("message"),
        HeaderValue::from_static("Registered successfully")
      );
      return res;
    }
  }
}

// input body: Ballot (JSON)
// output body: empty
async fn vote(
  body: web::Json<Ballot>,
  data: web::Data<AppState>
) -> impl Responder {
  println!("[info] /vote");
  if *data.election_phase.lock().unwrap() != ElectionPhase::Voting {
    let mut res = HttpResponse::new(StatusCode::BAD_REQUEST);
    res.headers_mut().insert(
      HeaderName::from_static("message"),
      HeaderValue::from_static("Election is not in Voting phase")
    );
    println!("wrong phase");
    return res; 
  } else {
    let ballot = body.into_inner();
    let user_votes = &mut *data.user_votes.lock().unwrap();
    if user_votes.contains(&ballot.authentication.login) {
      let mut res = HttpResponse::new(StatusCode::CONFLICT);
      res.headers_mut().insert(
        HeaderName::from_static("message"),
        HeaderValue::from_static("User already voted")
      );
      println!("double vote");
      return res;
    } else {
      match (*data.user_auth.lock().unwrap()).get(&ballot.authentication.login) {
        Some(pw_hash_saved) if hash(&ballot.authentication.password) == *pw_hash_saved => {
          let vote_data = &mut *data.vote_data.lock().unwrap();
          for shard in ballot.shards {
            println!("  {:?} -> {:?}", shard.candidateId, shard.grade);
            vote_data.update(
              shard.candidateId as usize,
              shard.grade.to_i8() as usize,
              |count| count + 1
            );
          }
          let number_votes_cast = &mut *data.number_votes_cast.lock().unwrap();
          *number_votes_cast += 1;
          user_votes.insert(ballot.authentication.login);
          let mut res = HttpResponse::new(StatusCode::OK);
          res.headers_mut().insert(
            HeaderName::from_static("message"),
            HeaderValue::from_static("Voted successfully")
          ); 
          return res; 
        },
        _ => {
          let mut res = HttpResponse::new(StatusCode::CONFLICT);
          res.headers_mut().insert(
            HeaderName::from_static("message"),
            HeaderValue::from_static("Wrong credentials")
          ); 
          println!("wrong credentials");
          return res;
        }
      }
    }
  }
}

// input body: Authentication (JSON)
// output body: empty
async fn forward(
  body: web::Json<Authentication>,
  data: web::Data<AppState>
) -> impl Responder {
  println!("[info] /forward");
  let auth = body.into_inner();
  let pw_hash = hash(&auth.password);
  if auth.login == *data.admin_login && data.admin_pw_hash == pw_hash {
    let mut phase = data.election_phase.lock().unwrap();
    match *phase {
      ElectionPhase::Register => *phase = ElectionPhase::Voting,
      ElectionPhase::Voting => {
        *phase = ElectionPhase::Results;
        let number_votes_cast = *data.number_votes_cast.lock().unwrap();
        if number_votes_cast > 0 {
          *data.vote_results.lock().unwrap() =
            major::compute_results(&*data.vote_data.lock().unwrap(), number_votes_cast); 
        }
      },
      _ => ()
    };
    return HttpResponse::new(StatusCode::OK);
  } else {
    let mut res = HttpResponse::new(StatusCode::CONFLICT);
    res.headers_mut().insert(
      HeaderName::from_static("message"),
      HeaderValue::from_static("Wrong admin credentials")
    );
    println!("wrong credentials");
    return res; 
  }
}
  
// input body: Authentication (JSON)
// output body: raw text
async fn users(
  body: web::Json<Authentication>,
  data: web::Data<AppState>
) -> impl Responder {
  println!("[info] /users");
  let auth = body.into_inner();
  let pw_hash = hash(&auth.password);
  if auth.login == *data.admin_login && data.admin_pw_hash == pw_hash {
    let registered_users = &*data.user_auth.lock().unwrap();
    let users_who_voted = &*data.user_votes.lock().unwrap();
    let mut res = String::from("registered:");
    for user_login in registered_users.keys() {
      if !users_who_voted.contains(user_login) {
        res += " ";
        res += user_login;
      }
    }
    res += "\nvoted:";
    for user_login in users_who_voted {
      res += " ";
      res += user_login;
    }
    return HttpResponse::Ok().body(res)
  } else {
    let mut res = HttpResponse::new(StatusCode::CONFLICT);
    res.headers_mut().insert(
      HeaderName::from_static("message"),
      HeaderValue::from_static("Wrong admin credentials")
    );
    println!("wrong credentials");
    return res; 
  }
}