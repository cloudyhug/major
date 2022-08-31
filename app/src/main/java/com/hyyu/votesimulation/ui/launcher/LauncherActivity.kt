package com.hyyu.votesimulation.ui.launcher

import android.os.Bundle
import androidx.activity.compose.setContent
import androidx.appcompat.app.AppCompatActivity
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.hilt.navigation.compose.hiltViewModel
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.navigation
import androidx.navigation.compose.rememberNavController
import com.hyyu.votesimulation.model.launcher.LauncherType
import com.hyyu.votesimulation.navigation.directions.launcher.LauncherNavigationDirections
import com.hyyu.votesimulation.navigation.handleNavigation
import com.hyyu.votesimulation.navigation.navigators.AppNavigator
import com.hyyu.votesimulation.ui.launcher.compose.Launcher
import com.hyyu.votesimulation.ui.theme.MajorTheme
import dagger.hilt.android.AndroidEntryPoint
import javax.inject.Inject

@AndroidEntryPoint
class LauncherActivity : AppCompatActivity() {

    companion object {
        val TAG: String = LauncherActivity::class.java.simpleName
    }

    @Inject
    lateinit var navigator: AppNavigator

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            MajorTheme {
                Navigation()
            }
        }
    }

    @Composable
    fun Navigation() {
        val navController = rememberNavController()

        navigator.commands.collectAsState().value.also { command ->
            navController.handleNavigation(command, navigator)
        }

        NavHost(
            navController = navController,
            startDestination = LauncherNavigationDirections.navigationRoute
        ) {
            navigation(
                route = LauncherNavigationDirections.navigationRoute,
                startDestination = LauncherNavigationDirections.login.destination
            ) {
                // Launcher
                composable(route = LauncherNavigationDirections.login.destination) {
                    Launcher(
                        hiltViewModel(),
                        LauncherType.LOGIN
                    )
                }

                // Signup
                composable(route = LauncherNavigationDirections.signup.destination) {
                    Launcher(
                        hiltViewModel(),
                        LauncherType.LOGIN
                    )
                }
            }
        }
    }

}
