package com.hyyu.votesimulation.network.response

import com.google.gson.annotations.Expose
import com.google.gson.annotations.SerializedName

data class CandidateObjectResponse(
  @SerializedName("id")
  @Expose
  val id: Int,

  @SerializedName("name")
  @Expose
  val name: String,

  @SerializedName("party")
  @Expose
  val party: PartyObjectResponse
)

data class PartyObjectResponse(
  @SerializedName("name")
  @Expose
  val name: String,

  @SerializedName("colour")
  @Expose
  val colour: String
)
