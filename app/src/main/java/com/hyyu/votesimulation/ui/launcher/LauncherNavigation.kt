package com.hyyu.votesimulation.ui.launcher

import androidx.hilt.navigation.compose.hiltViewModel
import androidx.navigation.NavGraphBuilder
import androidx.navigation.compose.composable
import androidx.navigation.compose.navigation
import com.hyyu.votesimulation.navigation.directions.launcher.LauncherNavigationDirections
import com.hyyu.votesimulation.navigation.navigators.LauncherNavigator
import com.hyyu.votesimulation.ui.launcher.compose.Launcher

fun NavGraphBuilder.launcherNavigation(launcherNavigator: LauncherNavigator) {
    navigation(
        route = LauncherNavigationDirections.navigationRoute,
        startDestination = LauncherNavigationDirections.authentication.destination
    ) {
        composable(route = LauncherNavigationDirections.authentication.destination) {
            Launcher(
                hiltViewModel(),
                launcherNavigator
            )
        }
    }
}
