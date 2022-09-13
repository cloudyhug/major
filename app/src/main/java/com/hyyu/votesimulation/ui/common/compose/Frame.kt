package com.hyyu.votesimulation.ui.common.compose

import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.material.Scaffold
import androidx.compose.material.SnackbarHost
import androidx.compose.material.rememberScaffoldState
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.ui.Modifier
import com.hyyu.votesimulation.model.common.snackbar.MajorSnackbarData
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.launch

@Composable
fun Frame(
    modifier: Modifier = Modifier,
    header: @Composable () -> Unit = {},
    onShowSnackbar: (() -> Unit)? = null,
    snackbarState: StateFlow<MajorSnackbarData?>? = null,
    content: @Composable (PaddingValues) -> Unit,
) {
    val coroutineScope = rememberCoroutineScope()
    val scaffoldState = rememberScaffoldState()

    val showSnackbar: (MajorSnackbarData) -> Unit = {
        coroutineScope.launch {
            onShowSnackbar?.invoke()

            scaffoldState.snackbarHostState.showSnackbar(
                message = it.message,
                actionLabel = it.type.name,
            )
        }
    }

    snackbarState?.collectAsState()?.value.also { data ->
        if (data != null) {
            showSnackbar(data)
        }
    }

    Scaffold(
        modifier = modifier,
        topBar = header,
        scaffoldState = scaffoldState,
        snackbarHost = {
            SnackbarHost(it) { data ->
                MajorSnackbar(data)
            }
        },
        content = content,
    )

}
