package com.hyyu.votesimulation.ui.common.preview

import androidx.compose.foundation.layout.defaultMinSize
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import com.hyyu.votesimulation.ui.common.compose.ButtonWithLoader
import com.hyyu.votesimulation.R.string
import com.hyyu.votesimulation.ui.theme.MajorTheme

@Preview
@Composable
fun ButtonWithLoaderPreviewLoading() {
    MajorTheme {
        ButtonWithLoader(
            modifier = Modifier,
            isLoading = true,
            title = stringResource(id = string.dialog_register_send)
        )
    }
}

@Preview
@Composable
fun ButtonWithLoaderPreviewIdle() {
    MajorTheme {
        ButtonWithLoader(
            modifier = Modifier.defaultMinSize(minHeight = 56.dp),
            isLoading = false,
            title = stringResource(id = string.dialog_register_send)
        )
    }
}
