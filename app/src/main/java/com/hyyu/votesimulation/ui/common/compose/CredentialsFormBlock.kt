package com.hyyu.votesimulation.ui.common.compose

import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.material.Icon
import androidx.compose.material.IconButton
import androidx.compose.material.OutlinedTextField
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.input.PasswordVisualTransformation
import androidx.compose.ui.text.input.VisualTransformation
import com.hyyu.votesimulation.R
import com.hyyu.votesimulation.ui.theme.MajorDimens
import com.hyyu.votesimulation.ui.theme.MajorFonts

@Composable
fun CredentialsFormBlock(
    login: String,
    onLoginChange: (String) -> Unit,
    password: String,
    onPasswordChange: (String) -> Unit
) {
    var passwordVisibility: Boolean by remember { mutableStateOf(false) }

    OutlinedTextField(
        modifier = Modifier.fillMaxWidth(MajorDimens.TextField.WIDTH_FRACTION_LAUNCHER),
        value = login,
        onValueChange = onLoginChange,
        textStyle = MajorFonts.formText,
        label = {
            Text(
                text = stringResource(R.string.activity_launcher_login_label),
                style = MajorFonts.formLabel
            )
        }
    )
    Spacer(modifier = Modifier.height(MajorDimens.Padding.normal))
    OutlinedTextField(
        modifier = Modifier.fillMaxWidth(MajorDimens.TextField.WIDTH_FRACTION_LAUNCHER),
        value = password,
        onValueChange = onPasswordChange,
        textStyle = if (passwordVisibility) MajorFonts.formText else MajorFonts.passwordText,
        visualTransformation = if (passwordVisibility) VisualTransformation.None else PasswordVisualTransformation(),
        label = {
            Text(
                text = stringResource(R.string.activity_launcher_password_label),
                style = MajorFonts.formLabel
            )
        },
        trailingIcon = {
            IconButton(onClick = { passwordVisibility = !passwordVisibility }) {
                Icon(
                    painter = painterResource(id = if (passwordVisibility) R.drawable.ic_visibility_on else R.drawable.ic_visibility_off),
                    contentDescription = stringResource(id = R.string.icon_visibility_content_description)
                )
            }
        }
    )
}
