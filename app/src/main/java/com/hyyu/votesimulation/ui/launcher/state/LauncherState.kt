package com.hyyu.votesimulation.ui.launcher.state

import com.hyyu.votesimulation.network.body.CredentialsObjectBody

class LauncherState(
    val isLoading: Boolean = false,
    val isFailure: Boolean = false,
    val isAuthenticated: Boolean = false,
    val credentials: CredentialsObjectBody? = null,
) {
    fun build(block: Builder.() -> Unit) = Builder(this).apply(block).build()

    class Builder(uiModel: LauncherState) {
        var isLoading = uiModel.isLoading
        var isFailure = uiModel.isFailure
        var isAuthenticated = uiModel.isAuthenticated
        var credentials = uiModel.credentials

        fun build(): LauncherState {
            return LauncherState(
                isLoading,
                isFailure,
                isAuthenticated,
                credentials
            )
        }
    }
}
