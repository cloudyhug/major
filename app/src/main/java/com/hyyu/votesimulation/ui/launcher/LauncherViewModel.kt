package com.hyyu.votesimulation.ui.launcher

import android.util.Log
import androidx.lifecycle.*
import com.hyyu.votesimulation.network.body.CredentialsObjectBody
import com.hyyu.votesimulation.network.response.ConnectionObjectResponse
import com.hyyu.votesimulation.repository.MainRepository
import com.hyyu.votesimulation.util.state.DataState
import com.hyyu.votesimulation.util.extension.isValidLogin
import dagger.hilt.android.lifecycle.HiltViewModel
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.flow.launchIn
import kotlinx.coroutines.flow.onEach
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import javax.inject.Inject

@HiltViewModel
class LauncherViewModel
@Inject constructor(
    private val mainRepository: MainRepository,
    private val savedStateHandle: SavedStateHandle
) : ViewModel() {

    companion object {
        val TAG: String = LauncherViewModel::class.java.simpleName
    }

    /* LiveData associated to user's registering */
    private val _registerDataState: MutableLiveData<DataState<Unit>> = MutableLiveData()
    val registerDataState: LiveData<DataState<Unit>>
        get() = _registerDataState

    /* LiveData associated to user's connection */
    private val _connectionDataState: MutableLiveData<DataState<ConnectionObjectResponse>> = MutableLiveData()
    val connectionDataState: LiveData<DataState<ConnectionObjectResponse>>
        get() = _connectionDataState

    lateinit var credentialsBody: CredentialsObjectBody

    enum class ValidatorCode {
        OK, LOGIN_NOT_VALID, PASSWORD_NOT_VALID
    }

    fun setStateEvent(event: LauncherStateEvent) {
        viewModelScope.launch {
            when (event) {
                is LauncherStateEvent.ConnectEvent -> withContext(Dispatchers.IO) { logInToUserAccount() }
                is LauncherStateEvent.RegisterEvent -> withContext(Dispatchers.IO) { registerNewUserAccount() }
            }
        }
    }

    private suspend fun logInToUserAccount() {
        mainRepository.logInToUserAccount(credentialsBody)
            .onEach { dataState ->
                _connectionDataState.postValue(dataState)
            }
            .launchIn(viewModelScope)
    }

    private suspend fun registerNewUserAccount() {
        mainRepository.registerNewUserAccount(credentialsBody)
            .onEach { dataState ->
                _registerDataState.value = dataState
            }
            .launchIn(viewModelScope)
    }

    fun validateCredentials(body: CredentialsObjectBody): ValidatorCode {
        return when {
            !body.login.isValidLogin() -> ValidatorCode.LOGIN_NOT_VALID
            body.password.isEmpty() -> {
                ValidatorCode.PASSWORD_NOT_VALID
            }
            else -> {
                ValidatorCode.OK
            }
        }
    }

    fun createCredentials(login: String, password: String) = CredentialsObjectBody(login, password, mainRepository.deviceName!!)
    fun getLocalAccessToken() = mainRepository.accessToken

}

sealed class LauncherStateEvent {
    object ConnectEvent : LauncherStateEvent()
    object RegisterEvent : LauncherStateEvent()
}
