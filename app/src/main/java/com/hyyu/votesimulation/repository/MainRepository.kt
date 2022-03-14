package com.hyyu.votesimulation.repository

import android.os.Build
import android.util.Log
import com.hyyu.votesimulation.database.BlogDao
import com.hyyu.votesimulation.database.CacheMapper
import com.hyyu.votesimulation.network.MajorApi
import com.hyyu.votesimulation.network.BlogMapper
import com.hyyu.votesimulation.network.body.CredentialsObjectBody
import com.hyyu.votesimulation.network.response.ConnectionObjectResponse
import com.hyyu.votesimulation.network.response.EmptyObjectResponse
import com.hyyu.votesimulation.prefs.Session
import com.hyyu.votesimulation.util.state.DataState
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.flow
import kotlin.random.Random

class MainRepository
constructor(
    private val blogDao: BlogDao,
    private val sessionPrefs: Session,
    private val majorApi: MajorApi,
    private val cacheMapper: CacheMapper,
    private val blogMapper: BlogMapper
) {

    companion object {
        val TAG: String = MainRepository::class.java.simpleName
    }

    init {
        if (sessionPrefs.deviceName.isNullOrEmpty()) {
            sessionPrefs.deviceName = "${Build.MANUFACTURER}-${Build.MODEL} ${Random.nextInt(10000)}"
        }
    }

    suspend fun logInToUserAccount(body: CredentialsObjectBody): Flow<DataState<ConnectionObjectResponse>> = flow {
        emit(DataState.Loading)
        delay(1000)
        try {
            val connectionResponse = majorApi.connect(body.login, body.password, sessionPrefs.deviceName!!)
                .also {
                    sessionPrefs.accessToken = it.accessToken
                    sessionPrefs.refreshToken = it.refreshToken
                }
            emit(DataState.Success(connectionResponse))
        } catch (e: Exception) {
            emit(DataState.Error(e))
        }
    }

    suspend fun registerNewUserAccount(body: CredentialsObjectBody): Flow<DataState<EmptyObjectResponse>> = flow {
        emit(DataState.Loading)
        delay(1000)
        try {
            Log.v(TAG, "sending register request")
            val registerResponse = majorApi.register(body.login, body.password, sessionPrefs.deviceName!!)
            Log.v(TAG, "response: $registerResponse")
            emit(DataState.Success(registerResponse))
        } catch (e: Exception) {
            Log.v(TAG, "error: ${e.message}")
            emit(DataState.Error(e))
        }
    }

}
