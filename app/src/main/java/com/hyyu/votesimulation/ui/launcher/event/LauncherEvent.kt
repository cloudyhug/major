package com.hyyu.votesimulation.ui.launcher.event

sealed class LauncherEvent {
    data class Signup(val login: String, val password: String): LauncherEvent()
    data class Login(val login: String, val password: String): LauncherEvent()
}
