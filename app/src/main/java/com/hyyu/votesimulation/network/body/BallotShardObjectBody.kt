package com.hyyu.votesimulation.network.body

import com.google.gson.annotations.Expose
import com.google.gson.annotations.SerializedName
import com.hyyu.votesimulation.model.Rating

data class BallotShardObjectBody(
  @SerializedName("id")
  @Expose
  val candidateId: Int,

  @SerializedName("rating")
  @Expose
  val candidateRating: Rating
)
