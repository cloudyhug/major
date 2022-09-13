package com.hyyu.votesimulation.ui.launcher

import android.os.Bundle
import androidx.activity.compose.setContent
import androidx.appcompat.app.AppCompatActivity
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.rememberNavController
import com.hyyu.votesimulation.navigation.directions.launcher.LauncherNavigationDirections
import com.hyyu.votesimulation.navigation.handleNavigation
import com.hyyu.votesimulation.navigation.navigators.AppNavigator
import com.hyyu.votesimulation.navigation.navigators.LauncherNavigator
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

    @Inject
    lateinit var launcherNavigator: LauncherNavigator

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
        NavHost(navController = navController,
            startDestination = LauncherNavigationDirections.navigationRoute
        ) {
            launcherNavigation(launcherNavigator)
        }
    }

}
