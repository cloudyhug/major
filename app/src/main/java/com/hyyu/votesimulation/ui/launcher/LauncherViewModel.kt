package com.hyyu.votesimulation.ui.launcher

import androidx.lifecycle.viewModelScope
import com.hyyu.votesimulation.navigation.BackCommand
import com.hyyu.votesimulation.navigation.directions.launcher.LauncherNavigationDirections
import com.hyyu.votesimulation.navigation.navigators.AppNavigator
import com.hyyu.votesimulation.network.DataState
import com.hyyu.votesimulation.network.body.CredentialsObjectBody
import com.hyyu.votesimulation.repository.MainRepository
import com.hyyu.votesimulation.ui.launcher.event.LauncherEvent
import com.hyyu.votesimulation.ui.launcher.event.LauncherEvent.*
import com.hyyu.votesimulation.ui.launcher.state.LauncherState
import com.hyyu.votesimulation.util.base.BaseViewModel
import dagger.hilt.android.lifecycle.HiltViewModel
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.launch
import javax.inject.Inject

@HiltViewModel
class LauncherViewModel @Inject constructor(
    private val mainRepository: MainRepository,
    private val appNavigator: AppNavigator
) : BaseViewModel() {

    companion object {
        val TAG: String = LauncherViewModel::class.java.simpleName
    }

    private val _uiState = MutableStateFlow(LauncherState())
    val uiState: StateFlow<LauncherState> = _uiState

    fun handleEvent(event: LauncherEvent) {
        when (event) {
            is Back -> navigateBack()
            is GoToSignup -> navigateToSignup()
            is Signup -> registerAccount(event)
            is Login -> logInToAccount(event)
        }
    }

    private fun navigateBack() {
        appNavigator.navigate(BackCommand)
    }

    private fun navigateToSignup() {
        appNavigator.navigate(LauncherNavigationDirections.signup)
    }

    private fun registerAccount(event: Signup) {
        val credentials = CredentialsObjectBody(event.login, event.password, "")
        viewModelScope.launch {
            mainRepository.registerNewUserAccount(credentials)
                .flowOn(Dispatchers.IO)
                .onEach { dataState ->
                    _uiState.value = _uiState.value.build {
                        isLoading = dataState is DataState.Loading
                        isFailure = dataState is DataState.Error
                        if (dataState is DataState.Success)
                            this.credentials = credentials
                    }
                }.launchIn(this)
        }
    }

    private fun logInToAccount(event: Login) {
        val credentials = CredentialsObjectBody(event.login, event.password, "")

        viewModelScope.launch {
            mainRepository.logInToUserAccount(credentials)
                .flowOn(Dispatchers.IO)
                .onEach { dataState ->
                    _uiState.value = _uiState.value.build {
                        isLoading = dataState is DataState.Loading
                        isFailure = dataState is DataState.Error
                        isAuthenticated = dataState is DataState.Success
                    }
                }.launchIn(this)
        }
    }

}
