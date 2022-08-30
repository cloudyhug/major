package com.hyyu.votesimulation.navigation

object BackCommand : NavigationCommand {
    override var arguments = emptyMap<String, String>()
    override val destination = "back"
}