package com.hyyu.votesimulation.navigation

sealed class Screen(val route: String) {

    object Authentication : Screen("authentication")
    object Signup : Screen("signup")
    object Login : Screen("login")

}
