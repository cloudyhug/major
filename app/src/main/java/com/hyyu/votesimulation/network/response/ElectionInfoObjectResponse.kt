package com.hyyu.votesimulation.network.response

import com.google.gson.annotations.Expose
import com.google.gson.annotations.SerializedName

data class ElectionInfoObjectResponse(
  @SerializedName("id")
  @Expose
  val id: Int,

  @SerializedName("name")
  @Expose
  val name: String,

  @SerializedName("isRunning")
  @Expose
  val isRunning: Boolean
)