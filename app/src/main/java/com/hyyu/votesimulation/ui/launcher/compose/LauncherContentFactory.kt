package com.hyyu.votesimulation.ui.launcher.compose

import androidx.activity.compose.BackHandler
import androidx.compose.foundation.Image
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import com.hyyu.votesimulation.ui.launcher.LauncherViewModel
import com.hyyu.votesimulation.R.drawable
import com.hyyu.votesimulation.R.string
import com.hyyu.votesimulation.model.launcher.LauncherType
import com.hyyu.votesimulation.ui.common.compose.ButtonWithLoader
import com.hyyu.votesimulation.ui.common.compose.CredentialsFormBlock
import com.hyyu.votesimulation.ui.launcher.event.LauncherEvent
import com.hyyu.votesimulation.ui.launcher.state.LauncherState
import com.hyyu.votesimulation.ui.theme.MajorDimens
import com.hyyu.votesimulation.ui.theme.MajorFonts

@Composable
fun Launcher(
    viewModel: LauncherViewModel,
    type: LauncherType
) {
    val state by viewModel.uiState.collectAsState()
    BackHandler(enabled = true) {
        viewModel.handleEvent(LauncherEvent.Back)
    }
    LaunchedEffect(key1 = state.credentials != null) {
        state.credentials?.let {
            viewModel.handleEvent(LauncherEvent.Login(it.login, it.password))
        }
    }
    when {
        state.isAuthenticated -> {
            // TODO: go to next screen
        }
        state.isFailure -> {
            // TODO: display Snackbar or Toast
        }
    }
    LauncherContent(
        viewModel = viewModel,
        type = type,
        state = state
    )
}

@Composable
private fun LauncherContent(
    viewModel: LauncherViewModel,
    type: LauncherType,
    state: LauncherState
) {
    Column(
        modifier = Modifier.fillMaxSize(),
        verticalArrangement = Arrangement.Center,
        horizontalAlignment = Alignment.CenterHorizontally
    ) {
        var login by remember { mutableStateOf("") }
        var password by remember { mutableStateOf("") }
        TitleBlock()
        Spacer(modifier = Modifier.height(MajorDimens.Padding.quadruple))
        CredentialsFormBlock(
            login,
            { login = it },
            password,
            { password = it }
        )
        Spacer(modifier = Modifier.height(MajorDimens.Padding.quadruple))
        ButtonWithLoader(
            modifier = Modifier
                .fillMaxWidth(MajorDimens.TextField.WIDTH_FRACTION_LAUNCHER),
            isLoading = state.isLoading,
            title = stringResource(
                id = if (type == LauncherType.LOGIN) string.activity_launcher_send
                else string.activity_launcher_signup
            ).uppercase(),
            onClickAction = {
                viewModel.handleEvent(
                    if (type == LauncherType.LOGIN) LauncherEvent.Login(login, password)
                    else LauncherEvent.Signup(login, password)
                )
            }
        )
        if (type == LauncherType.LOGIN) {
            Spacer(modifier = Modifier.height(MajorDimens.Padding.normal))
            SignupBlock { viewModel.handleEvent(LauncherEvent.GoToSignup) }
        }
    }
}

@Composable
private fun TitleBlock() {
    Column {
        Image(
            painter = painterResource(id = drawable.ic_logo),
            contentDescription = stringResource(string.icon_cake_content_description)
        )
        Spacer(modifier = Modifier.height(MajorDimens.Padding.normal))
        Text(
            text = stringResource(id = string.app_name),
            style = MajorFonts.appTitle,
            color = MaterialTheme.colors.primary
        )
    }
}

@Composable
private fun SignupBlock(
    onClick: () -> Unit
) {
    Column(
        modifier = Modifier
            .defaultMinSize(minHeight = MajorDimens.Clickable.minSize)
            .clickable { onClick.invoke() },
        verticalArrangement = Arrangement.Center
        ) {
        Text(
            text = stringResource(id = string.activity_launcher_no_account),
            style = MajorFonts.buttonText,
            color = MaterialTheme.colors.primary,
        )
    }
}
