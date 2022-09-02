package com.hyyu.votesimulation.ui.theme

import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp

object MajorDimens {

    /* Standard dimens for paddings */

    object Padding {
        val small = 4.dp
        val normal = 8.dp
        val double = 16.dp
        val quadruple = 32.dp
    }

    /* Elevation dimens */

    object Elevation {
        val buttonElevation = 2.dp
    }

    /* Text sizes */

    object TextSize {
        val h1 = 96.sp
        val h2 = 60.sp
        val h3 = 48.sp
        val h4 = 34.sp
        val h5 = 24.sp
        val h6 = 20.sp
        val regular1 = 16.sp
        val regular2 = 14.sp
        val caption = 12.sp
        val overline = 10.sp
    }

    /* Values for Launcher composable */

    object TextField {
        const val WIDTH_FRACTION_LAUNCHER = 0.65f
    }

    /* Values for ButtonWithLoader */

    object ButtonWithLoader {
        const val CORNER_RADIUS = 50
        val size = 56.dp
        val circularProgressIndicatorSize = 40.dp
        val circularProgressIndicatorStrokeWidth = 2.dp
    }

    object Clickable {
        val minSize = 48.dp
    }

    /* Positionning dimens */

    val centering = 48.dp

}
