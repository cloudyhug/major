package com.hyyu.votesimulation.model.common.snackbar

import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector

data class SnackbarAttr(
    val backgroundColor: Color,
    val image: ImageVector,
    val iconModifier: Modifier,
    val tint: Color
)
