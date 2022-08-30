package com.hyyu.votesimulation.ui.launcher.compose

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.material.Button
import androidx.compose.material.OutlinedTextField
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.input.PasswordVisualTransformation
import com.hyyu.votesimulation.ui.launcher.LauncherViewModel
import com.hyyu.votesimulation.R.string
import com.hyyu.votesimulation.ui.launcher.event.LauncherEvent

@Composable
fun Launcher(viewModel: LauncherViewModel) {
    var login by remember { mutableStateOf("") }
    var password by remember { mutableStateOf("") }

    Column(
        modifier = Modifier.fillMaxSize(),
        verticalArrangement = Arrangement.Center,
        horizontalAlignment = Alignment.CenterHorizontally
    ) {
    /*
        Image(
            painter = painterResource(),
            contentDescription = stringResource()
        )
    */
        OutlinedTextField(
            modifier = Modifier.fillMaxWidth(),
            value = login,
            onValueChange = { login = it },
            label = { Text(stringResource(string.activity_launcher_login_label)) }
        )
        OutlinedTextField(
            modifier = Modifier.fillMaxWidth(),
            value = password,
            onValueChange = { password = it },
            label = { Text(stringResource(string.activity_launcher_password_label)) },
            visualTransformation = PasswordVisualTransformation()
        )
        Button(
            modifier = Modifier.fillMaxWidth(),
            onClick = { viewModel.handleEvent(LauncherEvent.Signup(login, password)) }
        ) {
            Text(
                text = stringResource(id = string.activity_launcher_send)
            )
        }
    }
}
