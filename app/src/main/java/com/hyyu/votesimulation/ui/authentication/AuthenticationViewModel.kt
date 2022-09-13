package com.hyyu.votesimulation.ui.authentication

import androidx.lifecycle.viewModelScope
import com.hyyu.votesimulation.navigation.BackCommand
import com.hyyu.votesimulation.navigation.directions.launcher.AuthenticationNavigationDirections
import com.hyyu.votesimulation.navigation.navigators.LauncherNavigator
import com.hyyu.votesimulation.network.DataState
import com.hyyu.votesimulation.network.body.CredentialsObjectBody
import com.hyyu.votesimulation.repository.MainRepository
import com.hyyu.votesimulation.ui.authentication.event.AuthenticationEvent
import com.hyyu.votesimulation.ui.authentication.event.AuthenticationEvent.*
import com.hyyu.votesimulation.ui.authentication.state.AuthenticationState
import com.hyyu.votesimulation.util.base.BaseViewModel
import dagger.hilt.android.lifecycle.HiltViewModel
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.launch
import javax.inject.Inject

@HiltViewModel
class AuthenticationViewModel @Inject constructor(
    private val mainRepository: MainRepository,
    private val navigator: LauncherNavigator
) : BaseViewModel() {

    companion object {
        val TAG: String = AuthenticationViewModel::class.java.simpleName
    }

    private val _uiState = MutableStateFlow(AuthenticationState())
    val uiState: StateFlow<AuthenticationState> = _uiState

    fun handleEvent(event: AuthenticationEvent) {
        when (event) {
            is Back -> navigateBack()
            is GoToSignup -> navigateToSignup()
            is Signup -> registerAccount(event)
            is Login -> logInToAccount(event)
            is SnackbarMessage -> showSnackBar(event.message, event.type)
        }
    }

    private fun navigateBack() {
        navigator.navigate(BackCommand)
    }

    private fun navigateToSignup() {
        navigator.navigate(AuthenticationNavigationDirections.signup)
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
                        snackbarMessage = (dataState as? DataState.Error)?.exception?.message
                        if (!isFailure)
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
