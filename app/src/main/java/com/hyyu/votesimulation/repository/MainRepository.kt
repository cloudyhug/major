package com.hyyu.votesimulation.repository

import android.os.Build
import android.util.Log
import com.hyyu.votesimulation.network.DataState
import com.hyyu.votesimulation.network.MajorApi
import com.hyyu.votesimulation.network.body.CredentialsObjectBody
import com.hyyu.votesimulation.network.response.ConnectionObjectResponse
import com.hyyu.votesimulation.prefs.Session
import com.hyyu.votesimulation.util.const.CommonConst.LOADING_MINIMUM_DELAY
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.flow

@Suppress("BlockingMethodInNonBlockingContext")
class MainRepository
constructor(
    private val sessionPrefs: Session,
    private val majorApi: MajorApi
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
            sessionPrefs.deviceName =
                "${Build.MANUFACTURER}-${Build.MODEL} ${System.currentTimeMillis()}"
        }
    }

    suspend fun registerNewUserAccount(body: CredentialsObjectBody): Flow<DataState<Unit>> =
        flow {
            emit(DataState.Loading)
            delay(LOADING_MINIMUM_DELAY)
            try {
                body.clientId = sessionPrefs.deviceName!!
                Log.v(TAG, "${MajorApi.BASE_URL}${MajorApi.REGISTER}: body: $body")
                majorApi.register(body)
                    .apply {
                        if (isSuccessful) emit(DataState.Success(Unit))
                        else throw Exception(headers()["message"])
                    }
            } catch (e: Exception) {
                Log.e(TAG, "error: ${e.message}")
                emit(DataState.Error(e))
            }
        }

    suspend fun logInToUserAccount(body: CredentialsObjectBody): Flow<DataState<ConnectionObjectResponse>> =
        flow {
            emit(DataState.Loading)
            delay(LOADING_MINIMUM_DELAY)
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
                            } ?: throw Exception("Request body was null")
                        } else throw Exception(headers()["message"])
                    }
            } catch (e: Exception) {
                Log.e(TAG, "error: ${e.message}")
                emit(DataState.Error(e))
            }
        }

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
                        } else throw Exception(headers()["message"])
                    }
            }
        } catch (e: Exception) {
            Log.e(TAG, "error: ${e.message}")
            emit(DataState.Error(e))
        }
    }

}
