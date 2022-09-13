package com.hyyu.votesimulation.ui.launcher.compose

import androidx.activity.compose.BackHandler
import androidx.compose.foundation.layout.padding
import androidx.compose.material.Scaffold
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.ui.Modifier
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.rememberNavController
import com.hyyu.votesimulation.navigation.NavigationCommand
import com.hyyu.votesimulation.navigation.directions.launcher.AuthenticationNavigationDirections
import com.hyyu.votesimulation.navigation.handleNavigation
import com.hyyu.votesimulation.navigation.navigators.LauncherNavigator
import com.hyyu.votesimulation.ui.launcher.LauncherViewModel
import com.hyyu.votesimulation.ui.authentication.authenticationNavigation
import com.hyyu.votesimulation.ui.launcher.event.LauncherEvent

@Composable
fun Launcher(
    viewModel: LauncherViewModel,
    launcherNavigator: LauncherNavigator
) {
    BackHandler(enabled = true) {
        viewModel.handleEvent(LauncherEvent.Back)
    }

    LauncherScreenContent(launcherNavigator)
}

@Composable
fun LauncherScreenContent(
    launcherNavigator: LauncherNavigator
) {
    val navController = rememberNavController()
    launcherNavigator.commands.collectAsState().value.also { command: NavigationCommand ->
        navController.handleNavigation(command, launcherNavigator)
    }

    Scaffold { innerPadding ->
        val modifier = Modifier.padding(innerPadding)

        NavHost(
            navController = navController,
            startDestination = AuthenticationNavigationDirections.navigationRoute
        ) {
            authenticationNavigation(modifier)
        }
    }

}
