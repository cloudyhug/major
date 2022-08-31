package com.hyyu.votesimulation.ui.theme

import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.Font
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import com.hyyu.votesimulation.R

object MajorFonts {

    private val CeraPro = FontFamily(
        Font(R.font.cera_pro_regular),
        Font(R.font.cera_pro_light, weight = FontWeight.W300),
        Font(R.font.cera_pro_medium, weight = FontWeight.W700)
    )

    val formLabel = TextStyle(
        fontFamily = CeraPro,
        fontWeight = FontWeight.Normal,
        fontSize = MajorDimens.regular1
    )

    val formText = TextStyle(
        fontFamily = CeraPro,
        fontWeight = FontWeight.Normal,
        fontSize = MajorDimens.regular1
    )

    val button = TextStyle(
        fontFamily = CeraPro,
        fontWeight = FontWeight.Medium,
        fontSize = MajorDimens.regular1
    )

}
