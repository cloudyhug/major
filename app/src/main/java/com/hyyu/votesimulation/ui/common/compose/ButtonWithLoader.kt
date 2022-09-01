package com.hyyu.votesimulation.ui.common.compose

import androidx.compose.foundation.layout.defaultMinSize
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.Button
import androidx.compose.material.CircularProgressIndicator
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import com.hyyu.votesimulation.ui.theme.MajorColor
import com.hyyu.votesimulation.ui.theme.MajorDimens
import com.hyyu.votesimulation.ui.theme.MajorFonts

@Composable
fun ButtonWithLoader(
    modifier: Modifier,
    isLoading: Boolean,
    title: String,
    onClickAction: (() -> Unit)? = null
) {
    Button(
        modifier = modifier
            .defaultMinSize(minHeight = MajorDimens.ButtonWithLoader.size),
        shape = RoundedCornerShape(MajorDimens.ButtonWithLoader.CORNER_RADIUS),
        onClick = { onClickAction?.invoke() }
    ) {
        if (isLoading) {
            CircularProgressIndicator(
                modifier = Modifier
                    .size(MajorDimens.ButtonWithLoader.circularProgressIndicatorSize)
                    .padding(MajorDimens.Padding.normal),
                color = MajorColor.White,
                strokeWidth = MajorDimens.ButtonWithLoader.circularProgressIndicatorStrokeWidth
            )
        } else {
            Text(
                modifier = Modifier
                    .padding(MajorDimens.Padding.normal),
                text = title,
                style = MajorFonts.buttonText
            )
        }
    }
}
