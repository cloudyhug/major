package com.hyyu.votesimulation.network

import com.hyyu.votesimulation.network.response.BlogObjectResponse
import com.hyyu.votesimulation.network.response.ConnectionObjectResponse
import com.hyyu.votesimulation.network.response.EmptyObjectResponse
import retrofit2.http.Field
import retrofit2.http.FormUrlEncoded
import retrofit2.http.GET
import retrofit2.http.POST

interface MajorApi {

    companion object Road {
        //const val BASE_URL = "https://open-api.xyz/placeholder/"
        const val BASE_URL = "http:/192.168.1.24:8080/"

        const val CONNECT = "connect/"
        const val REGISTER = "register/"
    }

    @FormUrlEncoded
    @POST(CONNECT)
    suspend fun connect(
        @Field("login") login: String,
        @Field("password") password: String,
        @Field("clientId") deviceName: String
    ): ConnectionObjectResponse

    @FormUrlEncoded
    @POST(REGISTER)
    suspend fun register(
        @Field("login") login: String,
        @Field("password") password: String,
        @Field("clientId") deviceName: String
    ): EmptyObjectResponse

}
