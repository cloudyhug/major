package com.hyyu.votesimulation.ui.launcher.state

sealed class LauncherStateEvent {
    object ConnectEvent : LauncherStateEvent()
    object RegisterEvent : LauncherStateEvent()
}
