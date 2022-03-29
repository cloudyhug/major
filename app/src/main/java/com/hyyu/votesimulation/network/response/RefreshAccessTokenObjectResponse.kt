package com.hyyu.votesimulation.network.response

import com.google.gson.annotations.Expose
import com.google.gson.annotations.SerializedName

data class RefreshAccessTokenObjectResponse(
    @SerializedName("accessToken")
    @Expose
    val accessToken: String
)
