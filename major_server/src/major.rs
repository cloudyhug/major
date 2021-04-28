use crate::matrix::Matrix;
use crate::api::{Rating, CandidateScore};

pub fn compute_results(vote_data: &Matrix<u32>, number_votes_cast: u32) -> Vec<CandidateScore> {
  println!("debug results\n{:?}", vote_data);
  let mut res = Vec::new();
  res.reserve(vote_data.size.0);
  let elems = vote_data.elems();
  for row in 0..vote_data.size.0 {
    res.push(compute_candidate_score(row as u32, elems[row], number_votes_cast));
  }
  res.sort_by(|a, b| b.cmp(a));
  return res;
}

fn compute_candidate_score(id: u32, row: &[u32], number_votes_cast: u32) -> CandidateScore {
  let number_votes_cast_flt = number_votes_cast as f64;
  let fifty_percent_of_votes_cast = number_votes_cast as f64 / 2.0;
  let mut rating = Rating::Excellent;
  let mut accumulated_score = 0;
  let mut res = None;
  for score in row {
    let new_accumulated_score = accumulated_score + score;
    let new_accumulated_score_flt = new_accumulated_score as f64;
    if new_accumulated_score_flt >= fifty_percent_of_votes_cast {
      res = Some(CandidateScore {
        scoreId: id,
        medianGrade: rating,
        percentScore: new_accumulated_score_flt / number_votes_cast_flt * 100.0
      });
      break;
    } else {
      rating = rating.lesser();
      accumulated_score = new_accumulated_score;
    }
  }
  return res.unwrap();
}