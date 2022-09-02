package com.hyyu.votesimulation.ui.theme

import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.material.MaterialTheme
import androidx.compose.material.darkColors
import androidx.compose.material.lightColors
import androidx.compose.runtime.Composable

private val DarkColorPalette = darkColors(
    primary = MajorColor.Dark.primary,
    primaryVariant = MajorColor.Dark.primaryVariant,
    secondary = MajorColor.Dark.secondary
)

private val LightColorPalette = lightColors(
    primary = MajorColor.Light.primary,
    primaryVariant = MajorColor.Light.primary,
    secondary = MajorColor.Light.secondary
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
