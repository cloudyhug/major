package com.hyyu.votesimulation.navigation.navigators

import com.hyyu.votesimulation.navigation.NavigationCommand
import com.hyyu.votesimulation.navigation.directions.DefaultNavigationDirection
import com.hyyu.votesimulation.navigation.navigators.AppNavigator.Navigation.Startup
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow

class AppNavigator : NavigatorViewModel {
    private val _navigation = MutableStateFlow<Navigation>(Startup)
    val navigation: StateFlow<Navigation> = _navigation

    fun setNavigation(navigation: Navigation) {
        _navigation.value = navigation
    }

    sealed class Navigation {
        object Startup : Navigation()
        object Signup : Navigation()
        object Login : Navigation()
        object PasswordReset : Navigation()
        object Dashboard : Navigation()
    }

    private val _commands = MutableStateFlow(DefaultNavigationDirection.default)
    override val commands: StateFlow<NavigationCommand> = _commands

    override fun navigate(directions: NavigationCommand) {
        _commands.value = directions
    }

}
