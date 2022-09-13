package com.hyyu.votesimulation.ui.theme

import androidx.compose.ui.graphics.Color

object MajorColor {
    private val Orange200 = Color(0xFFFFCC80)
    private val Orange500 = Color(0xFFFF9800)
    private val Orange700 = Color(0xFFF57C00)
    private val BlueGrey200 = Color(0xFFB0BEC5)
    private val BlueGrey300 = Color(0xFF78909C)
    private val Red700 = Color(0xFFD32F2F)
    private val Green700 = Color(0xFF4CAF50)
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

    /* Error colors */

    object SnackbarMessage {
        object Error {
            val background = Red700
            val text = White
        }
        object Validation {
            val background = Green700
            val text = White
        }
    }

    /* ButtonWithLoader colors */

    object ButtonWithLoader {
        val indicator = White
        val text = White
    }

}
