package com.hyyu.votesimulation.network.response

import com.google.gson.annotations.Expose
import com.google.gson.annotations.SerializedName
import com.hyyu.votesimulation.model.Rating

data class BallotShardObjectResponse(
  @SerializedName("id")
  @Expose
  val candidateId: Int,

  @SerializedName("rating")
  @Expose
  val candidateRating: Rating
)