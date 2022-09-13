package com.hyyu.votesimulation.ui.authentication.event

import com.hyyu.votesimulation.model.common.snackbar.SnackbarType

sealed class AuthenticationEvent {
    object Back : AuthenticationEvent()
    object GoToSignup : AuthenticationEvent()
    data class Signup(val login: String, val password: String): AuthenticationEvent()
    data class Login(val login: String, val password: String): AuthenticationEvent()
    data class SnackbarMessage(val message: String, val type: SnackbarType): AuthenticationEvent()
}
