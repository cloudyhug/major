package com.hyyu.votesimulation.navigation.navigators

import com.hyyu.votesimulation.navigation.NavigationCommand
import kotlinx.coroutines.flow.StateFlow

interface NavigatorViewModel {
    val commands: StateFlow<NavigationCommand>
    fun navigate(directions: NavigationCommand)
}
