package com.hyyu.votesimulation.ui.launcher

import androidx.lifecycle.ViewModel
import com.hyyu.votesimulation.navigation.navigators.AppNavigator
import com.hyyu.votesimulation.ui.launcher.event.LauncherEvent
import com.hyyu.votesimulation.ui.launcher.event.LauncherEvent.Signup
import com.hyyu.votesimulation.ui.launcher.event.LauncherEvent.Login
import dagger.hilt.android.lifecycle.HiltViewModel
import javax.inject.Inject

@HiltViewModel
class LauncherViewModel @Inject constructor(
    private val appNavigator: AppNavigator
) : ViewModel() {

    fun handleEvent(event: LauncherEvent) {
        when (event) {
            is Signup -> registerAccount(event)
            is Login -> logInToAccount(event)
        }
    }

    private fun registerAccount(event: Signup) {

    }

    private fun logInToAccount(event: Login) {

    }

}
