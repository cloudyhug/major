package com.hyyu.votesimulation.network

import com.hyyu.votesimulation.network.body.CredentialsObjectBody
import com.hyyu.votesimulation.network.response.ConnectionObjectResponse
import retrofit2.Call
import retrofit2.http.*

interface MajorApi {

    companion object Road {
        //const val BASE_URL = "https://open-api.xyz/placeholder/"
        // const val BASE_URL = "http:/10.2.32.52:8080/"
        const val BASE_URL = "http:/192.168.1.24:8080/"

        const val CONNECT = "connect/"
        const val REGISTER = "register/"
    }

    @POST(CONNECT)
    suspend fun connect(
        @Body credentials: CredentialsObjectBody
    ): ConnectionObjectResponse

    @POST(REGISTER)
    suspend fun register(
        @Body credentials: CredentialsObjectBody
    ): Unit

}
