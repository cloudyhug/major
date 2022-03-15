package com.hyyu.votesimulation.network.body

data class CredentialsObjectBody (
    var login: String,
    var password: String,
    var clientId: String
)
