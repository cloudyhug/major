package com.hyyu.votesimulation.network.response

import com.google.gson.annotations.Expose
import com.google.gson.annotations.SerializedName

data class ConnectionObjectResponse(

    @SerializedName("refreshToken")
    @Expose
    val refreshToken: String,

    @SerializedName("accessToken")
    @Expose
    val accessToken: String

)
