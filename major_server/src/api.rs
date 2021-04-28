use serde_derive::{Deserialize, Serialize};
use std::cmp::Ordering;

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone)]
pub enum Rating {
  Excellent, Good, Satisfactory, Sufficient, Disappointing, Bad, Terrible
}

impl Rating {
  pub fn to_i8(&self) -> i8 {
    match self {
      Rating::Excellent => 0,
      Rating::Good => 1,
      Rating::Satisfactory => 2,
      Rating::Sufficient => 3,
      Rating::Disappointing => 4,
      Rating::Bad => 5,
      Rating::Terrible => 6
    }
  }

  pub fn lesser(&self) -> Self {
    match self {
      Rating::Excellent => Rating::Good,
      Rating::Good => Rating::Satisfactory,
      Rating::Satisfactory => Rating::Sufficient,
      Rating::Sufficient => Rating::Disappointing,
      Rating::Disappointing => Rating::Bad,
      Rating::Bad => Rating::Terrible,
      Rating::Terrible => Rating::Terrible
    }
  }
}

impl Ord for Rating {
  fn cmp(&self, other: &Self) -> Ordering {
    match self.to_i8() - other.to_i8() {
      0          => Ordering::Equal,
      n if n > 0 => Ordering::Less,
      _          => Ordering::Greater
    }
  }
}

impl PartialOrd for Rating {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

// /state response body

#[derive(Debug, Serialize)]
pub enum ElectionPhase {
  Register, Voting, Results
}

impl PartialEq for ElectionPhase {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
        (ElectionPhase::Register, ElectionPhase::Register)
      | (ElectionPhase::Voting, ElectionPhase::Voting)
      | (ElectionPhase::Results, ElectionPhase::Results) => true,
        _ => false
    }
  }
}
impl Eq for ElectionPhase {}

#[derive(PartialEq, Serialize, Clone)]
pub struct CandidateScore {
  pub scoreId: u32,
  pub medianGrade: Rating,
  pub percentScore: f64
}

impl Eq for CandidateScore {}

impl Ord for CandidateScore {
  fn cmp(&self, other: &Self) -> Ordering {
    match self.medianGrade.cmp(&other.medianGrade) {
      Ordering::Equal => self.percentScore.partial_cmp(&other.percentScore).unwrap(),
      ordering => ordering
    }
  }
}

impl PartialOrd for CandidateScore {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

#[derive(Serialize)]
pub struct ServerState {
  phase: ElectionPhase,
  info: Option<ElectionInfo>
}

impl ServerState {
  pub fn new(phase: ElectionPhase, info: Option<ElectionInfo>) -> Self {
    ServerState { phase: phase, info: info }
  }
}

#[derive(Serialize)]
pub struct ElectionInfo {
  pub participation: Option<f64>,
  pub scores: Option<Vec<CandidateScore>>,
  pub candidatesInfo: Vec<CandidateInfo>
}

#[derive(Deserialize, Serialize, Clone)]
pub struct CandidateInfo {
  pub id: u32,
  pub name: String,
  pub party: String,
  pub colour: String
}

// /vote request body

#[derive(Deserialize, Debug)]
pub struct VoteShard {
  pub candidateId: u32,
  pub grade: Rating
}

#[derive(Deserialize, Debug)]
pub struct Ballot {
  pub authentication: Authentication,
  pub shards: Vec<VoteShard>
}

#[derive(Debug, Deserialize)]
pub struct Authentication {
  pub login: String,
  pub password: String
}
