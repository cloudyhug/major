package com.hyyu.votesimulation.ui.authentication

import androidx.compose.ui.Modifier
import androidx.hilt.navigation.compose.hiltViewModel
import androidx.navigation.NavGraphBuilder
import androidx.navigation.compose.composable
import androidx.navigation.compose.navigation
import com.hyyu.votesimulation.model.launcher.AuthenticationType
import com.hyyu.votesimulation.navigation.directions.launcher.AuthenticationNavigationDirections
import com.hyyu.votesimulation.navigation.navigators.LauncherNavigator
import com.hyyu.votesimulation.ui.authentication.compose.Authentication

fun NavGraphBuilder.authenticationNavigation(
    modifier: Modifier
) {
    navigation(
        route = AuthenticationNavigationDirections.navigationRoute,
        startDestination = AuthenticationNavigationDirections.login.destination
    ) {
        composable(route = AuthenticationNavigationDirections.login.destination) {
            Authentication(
                hiltViewModel(),
                modifier,
                AuthenticationType.LOGIN
            )
        }

        composable(route = AuthenticationNavigationDirections.signup.destination) {
            Authentication(
                hiltViewModel(),
                modifier,
                AuthenticationType.SIGNUP
            )
        }
    }
}
