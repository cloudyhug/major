package com.hyyu.votesimulation.ui.theme

import androidx.compose.ui.graphics.Color

object MajorColor {
    val Orange200 = Color(0xFFFFCC80)
    val Orange500 = Color(0xFFFF9800)
    val Orange700 = Color(0xFFF57C00)
    val BlueGrey200 = Color(0xFFB0BEC5)
    val BlueGrey300 = Color(0xFF78909C)
    private val Red700 = Color(0xFFD32F2F)
    val White = Color(0xFFFAFAFA)

    /* Light theme colors */

    object Light {
        val primary = Orange500
        val primaryVariant = Orange700
        val secondary = BlueGrey200
    }

    /* Dark theme colors */

    object Dark {
        val primary = Orange200
        val primaryVariant = Orange700
        val secondary = Orange200
    }

    /* Error color */

    object SnackbarError {
        val background = Red700
        val text = White
    }

}
