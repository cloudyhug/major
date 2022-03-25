package com.hyyu.votesimulation.ui.theme

import androidx.compose.material.Typography
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.Font
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.sp
import com.hyyu.votesimulation.R

val CeraPro = FontFamily(
    Font(R.font.cera_pro_regular),
    Font(R.font.cera_pro_light, weight = FontWeight.W300),
    Font(R.font.cera_pro_medium, weight = FontWeight.W700)
)

val Typography = Typography(
    h1 = TextStyle(
        fontFamily = CeraPro,
        fontWeight = FontWeight.W300,
        fontSize = 96.sp
    ),
    h2 = TextStyle(
        fontFamily = CeraPro,
        fontWeight = FontWeight.W300,
        fontSize = 60.sp
    ),
    h3 = TextStyle(
        fontFamily = CeraPro,
        fontSize = 48.sp
    ),
    h4 = TextStyle(
        fontFamily = CeraPro,
        fontSize = 34.sp
    ),
    h5 = TextStyle(
        fontFamily = CeraPro,
        fontSize = 24.sp
    ),
    h6 = TextStyle(
        fontFamily = CeraPro,
        fontWeight = FontWeight.W700,
        fontSize = 20.sp
    ),
    subtitle1 = TextStyle(
        fontFamily = CeraPro,
        fontSize = 16.sp
    ),
    subtitle2 = TextStyle(
        fontFamily = CeraPro,
        fontWeight = FontWeight.W700,
        fontSize = 14.sp
    ),
    body1 = TextStyle(
        fontFamily = CeraPro,
        fontSize = 16.sp
    ),
    body2 = TextStyle(
        fontFamily = CeraPro,
        fontSize = 14.sp
    ),
    button = TextStyle(
        fontFamily = CeraPro,
        fontWeight = FontWeight.W700,
        fontSize = 14.sp
    ),
    caption = TextStyle(
        fontFamily = CeraPro,
        fontSize = 12.sp
    ),
    overline = TextStyle(
        fontFamily = CeraPro,
        fontSize = 10.sp
    )
)