package com.hyyu.votesimulation.network.body

import com.google.gson.annotations.Expose
import com.google.gson.annotations.SerializedName
import com.hyyu.votesimulation.util.const.CommonConst.EMPTY_STRING

data class CredentialsObjectBody (
    @SerializedName("login")
    @Expose
    var login: String,

    @SerializedName("password")
    @Expose
    var password: String,

    @SerializedName("clientID")
    @Expose
    var clientId: String = EMPTY_STRING
)
