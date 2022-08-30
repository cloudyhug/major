package com.hyyu.votesimulation.ui.theme

import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.material.MaterialTheme
import androidx.compose.material.darkColors
import androidx.compose.material.lightColors
import androidx.compose.runtime.Composable

private val DarkColorPalette = darkColors(
    primary = MajorColor.Orange200,
    primaryVariant = MajorColor.Orange700,
    secondary = MajorColor.Orange200
)

private val LightColorPalette = lightColors(
    primary = MajorColor.Orange500,
    primaryVariant = MajorColor.Orange700,
    secondary = MajorColor.BlueGrey200
)

@Composable
fun MajorTheme(
    darkTheme: Boolean = isSystemInDarkTheme(),
    content: @Composable () -> Unit
) {
    val colors = if (darkTheme) {
        DarkColorPalette
    } else {
        LightColorPalette
    }

    MaterialTheme(
        colors = colors,
        content = content
    )
}
