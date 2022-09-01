package com.hyyu.votesimulation.ui.theme

import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.Font
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.sp
import com.hyyu.votesimulation.R.font

object MajorFonts {

    private val CeraPro = FontFamily(
        Font(font.cera_pro_regular, weight = FontWeight.Normal),
        Font(font.cera_pro_light, weight = FontWeight.Light),
        Font(font.cera_pro_medium, weight = FontWeight.Medium),
        Font(font.cera_pro_bold, weight = FontWeight.Bold)
    )

    val appTitle = TextStyle(
        fontFamily = CeraPro,
        fontWeight = FontWeight.Medium,
        fontSize = MajorDimens.TextSize.h5
    )

    val formLabel = TextStyle(
        fontFamily = CeraPro,
        fontWeight = FontWeight.Normal,
        fontSize = MajorDimens.TextSize.regular1
    )

    val formText = TextStyle(
        fontFamily = CeraPro,
        fontWeight = FontWeight.Normal,
        fontSize = MajorDimens.TextSize.regular1
    )

    val passwordText = TextStyle(
        fontFamily = CeraPro,
        fontWeight = FontWeight.Normal,
        fontSize = MajorDimens.TextSize.regular1,
        letterSpacing = 4.sp
    )

    val buttonText = TextStyle(
        fontFamily = CeraPro,
        fontWeight = FontWeight.Medium,
        fontSize = MajorDimens.TextSize.regular1
    )

}
