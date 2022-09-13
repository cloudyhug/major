package com.hyyu.votesimulation.ui.launcher

import com.hyyu.votesimulation.navigation.BackCommand
import com.hyyu.votesimulation.navigation.navigators.AppNavigator
import com.hyyu.votesimulation.repository.MainRepository
import com.hyyu.votesimulation.ui.launcher.event.LauncherEvent
import com.hyyu.votesimulation.ui.launcher.event.LauncherEvent.Back
import com.hyyu.votesimulation.util.base.BaseViewModel
import dagger.hilt.android.lifecycle.HiltViewModel
import javax.inject.Inject

@HiltViewModel
class LauncherViewModel @Inject constructor(
    private val mainRepository: MainRepository,
    private val appNavigator: AppNavigator
) : BaseViewModel() {
    fun handleEvent(event: LauncherEvent) {
        when (event) {
            is Back -> navigateBack()
        }
    }

    private fun navigateBack() {
        appNavigator.navigate(BackCommand)
    }

}
