package com.hyyu.votesimulation.navigation.navigators

import com.hyyu.votesimulation.navigation.NavigationCommand
import com.hyyu.votesimulation.navigation.directions.DefaultNavigationDirection
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow

class LauncherNavigator : NavigatorViewModel {
    private val _commands = MutableStateFlow(DefaultNavigationDirection.default)
    override val commands: StateFlow<NavigationCommand> = _commands

    override fun navigate(directions: NavigationCommand) {
        _commands.value = directions
    }
}
