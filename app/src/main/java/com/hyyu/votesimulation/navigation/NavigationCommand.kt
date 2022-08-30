package com.hyyu.votesimulation.navigation

import androidx.navigation.NavHostController
import androidx.navigation.NavOptions
import com.hyyu.votesimulation.navigation.directions.DefaultNavigationDirection
import com.hyyu.votesimulation.navigation.navigators.NavigatorViewModel
import java.net.URLEncoder

interface NavigationCommand {
    var arguments: Map<String, String>
    val destination: String
}

private const val ROUTE_CHARSET = "utf-8"

fun NavHostController.handleNavigation(
    command: NavigationCommand,
    navigator: NavigatorViewModel
) {
    if (command is BackCommand) {
        popBackStack()
        navigator.navigate(DefaultNavigationDirection.default)
        return
    }
    if (command.destination.isNotEmpty()) {
        navigateWithOptions(command.destination, command.arguments)
    }
}

private fun NavHostController.navigateWithOptions(
    destination: String,
    arguments: Map<String, String>,
    navOptions: NavOptions? = null
) {
    var route = destination

    if (arguments.isNotEmpty()) {
        for (argument in arguments) {
            route = route.replace(
                oldValue = "{${argument.key}}",
                newValue = URLEncoder.encode(argument.value, ROUTE_CHARSET)
            )
        }
    }
    if (this.findDestination(destination) != null) {
        this.navigate(route, navOptions)
    }
}
