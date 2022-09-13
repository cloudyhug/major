package com.hyyu.votesimulation.ui.authentication.state

import com.hyyu.votesimulation.network.body.CredentialsObjectBody

class AuthenticationState(
    val isLoading: Boolean = false,
    val isFailure: Boolean = false,
    val isAuthenticated: Boolean = false,
    val snackbarMessage: String? = null,
    val credentials: CredentialsObjectBody? = null
) {
    fun build(block: Builder.() -> Unit) = Builder(this).apply(block).build()

    class Builder(uiModel: AuthenticationState) {
        var isLoading = uiModel.isLoading
        var isFailure = uiModel.isFailure
        var isAuthenticated = uiModel.isAuthenticated
        var credentials = uiModel.credentials
        var snackbarMessage = uiModel.snackbarMessage

        fun build(): AuthenticationState {
            return AuthenticationState(
                isLoading,
                isFailure,
                isAuthenticated,
                snackbarMessage,
                credentials
            )
        }
    }
}
