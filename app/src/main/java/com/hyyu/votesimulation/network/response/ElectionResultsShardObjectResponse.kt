package com.hyyu.votesimulation.network.response

import com.google.gson.annotations.Expose
import com.google.gson.annotations.SerializedName
import com.hyyu.votesimulation.model.Rating

data class ElectionResultsShardObjectResponse(
  @SerializedName("name")
  @Expose
  val candidateName: String,

  @SerializedName("rating")
  @Expose
  val rating: Rating,

  @SerializedName("score")
  @Expose
  val score: Double
)
