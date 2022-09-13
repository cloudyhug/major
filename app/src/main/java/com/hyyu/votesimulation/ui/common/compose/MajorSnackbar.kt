package com.hyyu.votesimulation.ui.common.compose

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.Card
import androidx.compose.material.Icon
import androidx.compose.material.SnackbarData
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import com.hyyu.votesimulation.model.common.snackbar.SnackbarType
import com.hyyu.votesimulation.model.common.snackbar.SnackbarType.ERROR
import com.hyyu.votesimulation.model.common.snackbar.toModel
import com.hyyu.votesimulation.ui.theme.MajorDimens
import com.hyyu.votesimulation.ui.theme.MajorFonts

@Composable
fun MajorSnackbar(data: SnackbarData) {
    MajorSnackBarContent(
        message = data.message,
        onClick = { data.dismiss() },
        type = data.actionLabel.snackBarLabelToType()
    )
}

@Composable
private fun MajorSnackBarContent(message: String, onClick: (() -> Unit)? = null, type: SnackbarType = ERROR) {
    val snackBarModel = type.toModel()
    Row(
        horizontalArrangement = Arrangement.Center,
        modifier = Modifier.fillMaxWidth()
    ) {
        Card(
            elevation = MajorDimens.Padding.medium,
            shape = RoundedCornerShape(MajorDimens.Padding.medium),
            backgroundColor = snackBarModel.backgroundColor,
            modifier = Modifier
                .padding(MajorDimens.Padding.double)
                .clickable { onClick?.invoke() }
        ) {
            Row(
                modifier = Modifier.padding(MajorDimens.Padding.double),
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.spacedBy(MajorDimens.Padding.medium),
            ) {
                Icon(
                    imageVector = snackBarModel.image,
                    contentDescription = null,
                    tint = snackBarModel.tint,
                    modifier = snackBarModel.iconModifier,
                )
                Text(
                    text = message,
                    color = snackBarModel.tint,
                    style = MajorFonts.snackbarText
                )
            }
        }
    }
}

/**
 *  Extension to transform the snackbar type as string into a SnackbarType enum
 */
private fun String?.snackBarLabelToType(): SnackbarType {
    return SnackbarType.getTypeByName(this)
}
