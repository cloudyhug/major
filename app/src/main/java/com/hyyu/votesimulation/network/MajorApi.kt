package com.hyyu.votesimulation.network

import com.hyyu.votesimulation.network.body.CredentialsObjectBody
import com.hyyu.votesimulation.network.response.ConnectionObjectResponse
import com.hyyu.votesimulation.network.response.RegisterObjectResponse
import retrofit2.http.*

interface MajorApi {

    companion object Road {
        //const val BASE_URL = "https://open-api.xyz/placeholder/"
        const val BASE_URL = "http:/10.2.32.52:8080/"

        const val CONNECT = "connect/"
        const val REGISTER = "register/"
    }

    @POST(CONNECT)
    suspend fun connect(
        @Field("login") login: String,
        @Field("password") password: String,
        @Field("clientId") deviceName: String
    ): ConnectionObjectResponse

    @POST(REGISTER)
    suspend fun register(
        @Body credentials: CredentialsObjectBody
    ): RegisterObjectResponse

}
