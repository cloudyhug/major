package com.hyyu.votesimulation.navigation.directions.launcher

import com.hyyu.votesimulation.navigation.NavigationCommand
import com.hyyu.votesimulation.navigation.Screen


object LauncherNavigationDirections {

    const val navigationRoute = "nav_launcher"

    val launcher = object : NavigationCommand {
        override var arguments = emptyMap<String, String>()
        override val destination = Screen.Launch.route
    }

    val signup = object : NavigationCommand {
        override var arguments = emptyMap<String, String>()
        override val destination = Screen.Signup.route
    }

    val login = object : NavigationCommand {
        override var arguments = emptyMap<String, String>()
        override val destination = Screen.Login.route
    }

}
