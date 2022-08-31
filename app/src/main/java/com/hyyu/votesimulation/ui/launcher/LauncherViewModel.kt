package com.hyyu.votesimulation.ui.launcher

import android.util.Log
import androidx.lifecycle.ViewModel
import com.hyyu.votesimulation.navigation.directions.launcher.LauncherNavigationDirections
import com.hyyu.votesimulation.navigation.navigators.AppNavigator
import com.hyyu.votesimulation.ui.launcher.event.LauncherEvent
import com.hyyu.votesimulation.ui.launcher.event.LauncherEvent.GoToSignup
import com.hyyu.votesimulation.ui.launcher.event.LauncherEvent.Login
import com.hyyu.votesimulation.ui.launcher.event.LauncherEvent.Signup
import dagger.hilt.android.lifecycle.HiltViewModel
import javax.inject.Inject

@HiltViewModel
class LauncherViewModel @Inject constructor(
    private val appNavigator: AppNavigator
) : ViewModel() {

    companion object {
        val TAG: String = LauncherViewModel::class.java.simpleName
    }

    fun handleEvent(event: LauncherEvent) {
        when (event) {
            is GoToSignup -> navigateToSignup()
            is Signup -> registerAccount(event)
            is Login -> logInToAccount(event)
        }
    }

    private fun navigateToSignup() {
        appNavigator.navigate(LauncherNavigationDirections.signup)
    }

    private fun registerAccount(event: Signup) {
    }

    private fun logInToAccount(event: Login) {
        Log.v("$TAG/Login", "login: ${event.login}")
        Log.v("$TAG/Login", "password: ${event.password}")
    }

}
