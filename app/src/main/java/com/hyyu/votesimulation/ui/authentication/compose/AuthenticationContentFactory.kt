package com.hyyu.votesimulation.ui.authentication.compose

import android.util.Log
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.Image
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import com.hyyu.votesimulation.R
import com.hyyu.votesimulation.model.common.snackbar.SnackbarType
import com.hyyu.votesimulation.model.launcher.AuthenticationType
import com.hyyu.votesimulation.ui.common.compose.ButtonWithLoader
import com.hyyu.votesimulation.ui.common.compose.CredentialsFormBlock
import com.hyyu.votesimulation.ui.common.compose.Frame
import com.hyyu.votesimulation.ui.authentication.AuthenticationViewModel
import com.hyyu.votesimulation.ui.authentication.event.AuthenticationEvent
import com.hyyu.votesimulation.ui.authentication.state.AuthenticationState
import com.hyyu.votesimulation.ui.theme.MajorDimens
import com.hyyu.votesimulation.ui.theme.MajorFonts

@Composable
fun Authentication(
    viewModel: AuthenticationViewModel,
    modifier: Modifier = Modifier,
    type: AuthenticationType
) {
    val state by viewModel.uiState.collectAsState()
    BackHandler(enabled = true) {
        viewModel.handleEvent(AuthenticationEvent.Back)
    }

    when {
        state.isAuthenticated -> {
            state.snackbarMessage?.let {
                viewModel.handleEvent(AuthenticationEvent.SnackbarMessage(it, SnackbarType.VALIDATION))
            }
            // TODO: launch main screen from LauncherNavigator
        }
        state.isFailure -> {
            state.snackbarMessage?.let {
                viewModel.handleEvent(AuthenticationEvent.SnackbarMessage(it, SnackbarType.ERROR))
            }
        }
    }

    LaunchedEffect(key1 = state.credentials != null) {
        Log.v("Authentication", "credentials: ${state.credentials}")
        state.credentials?.let {
            viewModel.handleEvent(AuthenticationEvent.Login(it.login, it.password))
        }
    }

    AuthenticationContent(
        viewModel = viewModel,
        modifier = modifier,
        type = type,
        state = state
    )
}

@Composable
private fun AuthenticationContent(
    viewModel: AuthenticationViewModel,
    modifier: Modifier,
    type: AuthenticationType,
    state: AuthenticationState
) {
    Frame(
        snackbarState = viewModel.snackbarState,
        onShowSnackbar = { viewModel.onSnackbarDisplayed() },
        modifier = modifier,
    ) {
        AuthenticationForm(
            viewModel = viewModel,
            type = type,
            state = state
        )
    }
}

@Composable
fun AuthenticationForm(
    viewModel: AuthenticationViewModel,
    type: AuthenticationType,
    state: AuthenticationState
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
                id = if (type == AuthenticationType.LOGIN) R.string.activity_launcher_send
                else R.string.activity_launcher_signup
            ).uppercase(),
            onClickAction = {
                Log.v("AuthenticationForm", "authentication button clicked, type = $type")
                viewModel.handleEvent(
                    if (type == AuthenticationType.LOGIN) AuthenticationEvent.Login(login, password)
                    else AuthenticationEvent.Signup(login, password)
                )
            }
        )
        if (type == AuthenticationType.LOGIN) {
            Spacer(modifier = Modifier.height(MajorDimens.Padding.normal))
            SignupBlock { viewModel.handleEvent(AuthenticationEvent.GoToSignup) }
        }
    }
}

@Composable
private fun TitleBlock() {
    Column {
        Image(
            painter = painterResource(id = R.drawable.ic_logo),
            contentDescription = stringResource(R.string.icon_cake_content_description)
        )
        Spacer(modifier = Modifier.height(MajorDimens.Padding.normal))
        Text(
            text = stringResource(id = R.string.app_name),
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
            text = stringResource(id = R.string.activity_launcher_no_account),
            style = MajorFonts.buttonText,
            color = MaterialTheme.colors.primary,
        )
    }
}
