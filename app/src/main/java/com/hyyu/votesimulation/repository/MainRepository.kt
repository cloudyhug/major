package com.hyyu.votesimulation.repository

import android.os.Build
import android.util.Log
import com.hyyu.votesimulation.database.BlogDao
import com.hyyu.votesimulation.database.CacheMapper
import com.hyyu.votesimulation.model.Election
import com.hyyu.votesimulation.network.MajorApi
import com.hyyu.votesimulation.network.ElectionMapper
import com.hyyu.votesimulation.network.body.CredentialsObjectBody
import com.hyyu.votesimulation.network.response.ConnectionObjectResponse
import com.hyyu.votesimulation.network.response.ElectionInfoObjectResponse
import com.hyyu.votesimulation.prefs.Session
import com.hyyu.votesimulation.util.state.DataState
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.flow.flowOn

@Suppress("BlockingMethodInNonBlockingContext")
class MainRepository
constructor(
    private val blogDao: BlogDao,
    private val sessionPrefs: Session,
    private val majorApi: MajorApi,
    private val cacheMapper: CacheMapper,
    private val electionMapper: ElectionMapper
) {

    companion object {
        val TAG: String = MainRepository::class.java.simpleName
    }

    val deviceName: String?
        get() = sessionPrefs.deviceName

    val accessToken: String?
        get() = sessionPrefs.accessToken

    init {
        if (sessionPrefs.deviceName.isNullOrEmpty()) {
            sessionPrefs.deviceName = "${Build.MANUFACTURER}-${Build.MODEL} ${System.currentTimeMillis()}"
        }
    }

    suspend fun registerNewUserAccount(body: CredentialsObjectBody): Flow<DataState<String>> = flow {
        emit(DataState.Loading)
        delay(1000)
        try {
            body.clientId = sessionPrefs.deviceName!!
            Log.v(TAG, "${MajorApi.BASE_URL}${MajorApi.REGISTER}: body: $body")
            majorApi.register(body)
                .apply {
                    if (isSuccessful) emit(DataState.Success(body.login))
                    else throw Exception(headers()["message"])
                }
        } catch (e: Exception) {
            Log.e(TAG, "error: ${e.message}")
            emit(DataState.Error(e))
        }
    }.flowOn(Dispatchers.IO)

    suspend fun logInToUserAccount(body: CredentialsObjectBody): Flow<DataState<ConnectionObjectResponse>> = flow {
        emit(DataState.Loading)
        delay(1000)
        try {
            body.clientId = sessionPrefs.deviceName!!
            Log.v(TAG, "${MajorApi.BASE_URL}${MajorApi.CONNECT}: body: $body")
            majorApi.connect(body)
                    .apply {
                        if (isSuccessful) {
                            body()?.let {
                                sessionPrefs.accessToken = it.accessToken
                                sessionPrefs.refreshToken = it.refreshToken
                                emit(DataState.Success(it))
                            } ?: throw Exception("Response body was null")
                        } else throw Exception(headers()["message"])
                    }
        } catch (e: Exception) {
            Log.e(TAG, "error: ${e.message}")
            emit(DataState.Error(e))
        }
    }.flowOn(Dispatchers.IO)

    suspend fun refreshAccessToken(): Flow<DataState<Unit>> = flow {
        try {
            sessionPrefs.refreshToken?.let {
                majorApi.refreshAccessToken(it)
                    .apply {
                        if (isSuccessful) {
                            body()?.let { token ->
                                sessionPrefs.accessToken = token
                                emit(DataState.Success(Unit))
                            } ?: throw Exception("Request body was null")
                        }
                        else throw Exception(headers()["message"])
                    }
            }
        } catch (e: Exception) {
            Log.e(TAG, "error: ${e.message}")
            emit(DataState.Error(e))
        }
    }.flowOn(Dispatchers.IO)

    suspend fun getElections(): Flow<DataState<List<Election>>> = flow {
        emit(DataState.Loading)
        delay(1000)
        try {
            Log.v(TAG, "${MajorApi.BASE_URL}${MajorApi.ELECTIONS}")
            val electionsResponse = majorApi.elections()
                .apply {
                    if (isSuccessful) {
                        body()?.let {
                            emit(DataState.Success(electionMapper.mapFromEntityList(it)))
                        } ?: throw Exception("Response body was null")
                    }
                    else throw Exception(headers()["message"])
                }
        } catch (e: Exception) {
            Log.e(TAG, "error: ${e.message}")
            emit(DataState.Error(e))
        }
    }.flowOn(Dispatchers.IO)

}
