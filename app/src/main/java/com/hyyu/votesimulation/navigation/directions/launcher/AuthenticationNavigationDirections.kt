package com.hyyu.votesimulation.navigation.directions.launcher

import com.hyyu.votesimulation.navigation.NavigationCommand
import com.hyyu.votesimulation.navigation.Screen


object AuthenticationNavigationDirections {

    const val navigationRoute = "nav_authentication"

    val login = object : NavigationCommand {
        override var arguments = emptyMap<String, String>()
        override val destination = Screen.Login.route
    }

    val signup = object : NavigationCommand {
        override var arguments = emptyMap<String, String>()
        override val destination = Screen.Signup.route
    }

}
