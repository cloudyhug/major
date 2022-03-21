package com.hyyu.votesimulation.network

import com.hyyu.votesimulation.network.body.BallotShardObjectBody
import com.hyyu.votesimulation.network.body.CredentialsObjectBody
import com.hyyu.votesimulation.network.response.*
import retrofit2.Call
import retrofit2.Response
import retrofit2.http.*

interface MajorApi {

    companion object Road {
        // const val BASE_URL = "http:/192.168.1.17:8080"
        const val BASE_URL = "http:/10.2.32.72:8080"

        const val CONNECT = "/connect"
        const val REGISTER = "/register"
        const val REFRESH_TOKEN = "/refreshToken"
        const val ELECTIONS = "/elections"
        const val ELECTION_INFO = "/electionInfo/{eID}"
        const val ELECTION_RESULTS = "/electionResults/{eID}"
        const val MY_VOTE = "/myVote/{eID}"
        const val VOTE = "/vote/{eID}"
    }

    @POST(CONNECT)
    suspend fun connect(
        @Body credentials: CredentialsObjectBody
    ): Response<ConnectionObjectResponse>

    @POST(REGISTER)
    suspend fun register(
        @Body credentials: CredentialsObjectBody
    ): Response<Unit>

    @GET(REFRESH_TOKEN)
    suspend fun refreshToken(
        @Body token: String
    ): Call<String>

    @GET(ELECTIONS)
    suspend fun elections(
        @Header("accessToken") accessToken: String
    ): Call<List<ElectionInfoObjectResponse>>

    @GET(ELECTION_INFO)
    suspend fun electionInfo(
        @Header("accessToken") accessToken: String,
        @Path("eID") electionID: Int
    ): Call<List<CandidateObjectResponse>>

    @GET(ELECTION_RESULTS)
    suspend fun electionResults(
        @Header("accessToken") accessToken: String,
        @Path("eID") electionID: Int
    ): Call<List<ElectionResultsShardObjectResponse>>

    @GET(MY_VOTE)
    suspend fun myVote(
        @Header("accessToken") accessToken: String,
        @Path("eID") electionID: Int
    ): Call<List<BallotShardObjectResponse>>

    @POST(VOTE)
    suspend fun vote(
        @Header("accessToken") accessToken: String,
        @Path("eID") electionID: Int,
        @Body ballot: List<BallotShardObjectBody>
    )
}
