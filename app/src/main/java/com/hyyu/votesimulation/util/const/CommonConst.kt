package com.hyyu.votesimulation.util.const

import com.hyyu.votesimulation.network.MajorApi

object CommonConst {
    const val EMPTY_STRING = ""

    const val RETROFIT_TIMEOUT = 10L
    const val LOADING_MINIMUM_DELAY = 1000L
    const val RETROFIT_REQUEST_TIMEOUT_MESSAGE = "Failed to connect to ${MajorApi.IP_ADDRESS}"

    const val REGISTER_ERROR_CREDENTIALS = "UnacceptableCredentials"
    const val REGISTER_ERROR_USER_ALREADY_REGISTERED = "UserAlreadyRegistered"

}
