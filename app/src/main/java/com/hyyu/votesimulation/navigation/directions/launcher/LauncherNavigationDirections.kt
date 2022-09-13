package com.hyyu.votesimulation.navigation.directions.launcher

import com.hyyu.votesimulation.navigation.NavigationCommand
import com.hyyu.votesimulation.navigation.Screen

object LauncherNavigationDirections {

    const val navigationRoute = "nav_launcher"

    val authentication = object : NavigationCommand {
        override var arguments = emptyMap<String, String>()
        override val destination = Screen.Authentication.route
    }

    val main = object : NavigationCommand {
        override var arguments = emptyMap<String, String>()
        override val destination = Screen.Signup.route
    }

}
