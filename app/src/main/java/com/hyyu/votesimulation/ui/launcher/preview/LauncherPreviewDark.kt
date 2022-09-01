package com.hyyu.votesimulation.ui.launcher.preview

import androidx.compose.runtime.Composable
import androidx.compose.ui.tooling.preview.Preview
import androidx.hilt.navigation.compose.hiltViewModel
import com.hyyu.votesimulation.model.launcher.LauncherType
import com.hyyu.votesimulation.ui.launcher.compose.Launcher
import com.hyyu.votesimulation.ui.theme.MajorTheme

@Preview
@Composable
fun LauncherPreviewDark() {
    MajorTheme(darkTheme = true) {
        Launcher(
            hiltViewModel(),
            LauncherType.LOGIN
        )
    }
}
