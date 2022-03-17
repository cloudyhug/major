package com.hyyu.votesimulation.repository

import android.os.Build
import android.util.Log
import com.hyyu.votesimulation.database.BlogDao
import com.hyyu.votesimulation.database.CacheMapper
import com.hyyu.votesimulation.network.MajorApi
import com.hyyu.votesimulation.network.BlogMapper
import com.hyyu.votesimulation.network.body.CredentialsObjectBody
import com.hyyu.votesimulation.network.response.ConnectionObjectResponse
import com.hyyu.votesimulation.prefs.Session
import com.hyyu.votesimulation.util.state.DataState
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.flow

@Suppress("BlockingMethodInNonBlockingContext")
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

    val deviceName: String?
        get() = sessionPrefs.deviceName

    val accessToken: String?
        get() = sessionPrefs.accessToken

    init {
        if (sessionPrefs.deviceName.isNullOrEmpty()) {
            sessionPrefs.deviceName = "${Build.MANUFACTURER}-${Build.MODEL} ${System.currentTimeMillis()}"
        }
    }

    suspend fun logInToUserAccount(body: CredentialsObjectBody): Flow<DataState<ConnectionObjectResponse>> = flow {
        emit(DataState.Loading)
        delay(1000)
        try {
            body.clientId = sessionPrefs.deviceName!!
            Log.v(TAG, "${MajorApi.BASE_URL}${MajorApi.CONNECT}: body: $body")
            val connectionResponse =
                majorApi.connect(
                    body
                ).apply {
                    sessionPrefs.accessToken = accessToken
                    sessionPrefs.refreshToken = refreshToken
                    emit(DataState.Success(this))
                }
        } catch (e: Exception) {
            Log.e(TAG, "error: ${e.message}")
            emit(DataState.Error(e))
        }
    }

    suspend fun registerNewUserAccount(body: CredentialsObjectBody): Flow<DataState<Unit>> = flow {
        emit(DataState.Loading)
        delay(1000)
        try {
            body.clientId = sessionPrefs.deviceName!!
            Log.v(TAG, "${MajorApi.BASE_URL}${MajorApi.REGISTER}: body: $body")
            val registerResponse = majorApi.register(body)
                .apply { emit(DataState.Success(Unit)) }
        } catch (e: Exception) {
            Log.e(TAG, "error: ${e.message}")
            emit(DataState.Error(e))
        }
    }

}
