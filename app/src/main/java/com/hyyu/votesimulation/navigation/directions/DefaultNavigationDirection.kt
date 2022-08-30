package com.hyyu.votesimulation.navigation.directions

import com.hyyu.votesimulation.navigation.NavigationCommand

object DefaultNavigationDirection {
    val default = object: NavigationCommand {
        override var arguments = emptyMap<String, String>()
        override val destination = ""
    }

}
