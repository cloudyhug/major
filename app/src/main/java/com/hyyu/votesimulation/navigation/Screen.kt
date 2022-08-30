package com.hyyu.votesimulation.navigation

sealed class Screen(val route: String) {

    object Launch : Screen("launch")
    object Signup : Screen("signup")
    object Login : Screen("login")

}
