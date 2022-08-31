package com.hyyu.votesimulation.ui.launcher.compose

import androidx.compose.foundation.Image
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
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
import com.hyyu.votesimulation.ui.common.compose.CredentialsFormBlock
import com.hyyu.votesimulation.ui.launcher.event.LauncherEvent
import com.hyyu.votesimulation.ui.theme.MajorColor
import com.hyyu.votesimulation.ui.theme.MajorDimens
import com.hyyu.votesimulation.ui.theme.MajorFonts

@Composable
fun Launcher(
    viewModel: LauncherViewModel,
    type: LauncherType
) {
    Column(
        modifier = Modifier.fillMaxSize(),
        verticalArrangement = Arrangement.Center,
        horizontalAlignment = Alignment.CenterHorizontally,

    ) {
        var login by remember { mutableStateOf("") }
        var password by remember { mutableStateOf("") }

        TitleBlock()
        Spacer(modifier = Modifier.height(MajorDimens.quad))
        CredentialsFormBlock(
            login,
            { login = it },
            password,
            { password = it }
        )
        Spacer(modifier = Modifier.height(MajorDimens.quad))
        Button(
            modifier = Modifier.fillMaxWidth(MajorDimens.launcherTextFieldWidthFraction),
            shape = RoundedCornerShape(50),
            onClick = {
                viewModel.handleEvent(LauncherEvent.Login(login, password))
            }
        ) {
            Row(modifier = Modifier.padding(MajorDimens.normal)) {
                Text(
                    text = stringResource(
                        id = if (type == LauncherType.SIGNUP) string.activity_launcher_signup
                        else string.activity_launcher_send
                    ).uppercase(),
                    style = MajorFonts.buttonText
                )
            }
        }
        Spacer(modifier = Modifier.height(MajorDimens.double))
        Text(
            modifier = Modifier.clickable { viewModel.handleEvent(LauncherEvent.GoToSignup) },
            text = stringResource(id = string.activity_launcher_no_account),
            style = MajorFonts.buttonText,
            color = MajorColor.Orange500,
        )
    }
}

@Composable
private fun TitleBlock() {
    Column {
        Image(
            painter = painterResource(id = drawable.ic_logo),
            contentDescription = stringResource(string.icon_cake_content_description)
        )
        Spacer(modifier = Modifier.height(MajorDimens.normal))
        Text(
            text = stringResource(id = string.app_name),
            style = MajorFonts.appTitle,
            color = MajorColor.Orange500
        )
    }
}
