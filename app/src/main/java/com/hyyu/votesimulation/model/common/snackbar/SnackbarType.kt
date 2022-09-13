package com.hyyu.votesimulation.model.common.snackbar

import androidx.compose.foundation.layout.size
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Info
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.rotate
import com.hyyu.votesimulation.ui.theme.MajorColor
import com.hyyu.votesimulation.ui.theme.MajorDimens

enum class SnackbarType {
    VALIDATION, ERROR;

    companion object {
        fun getTypeByName(name: String?): SnackbarType {
            name?.let {
                return try {
                    valueOf(name.uppercase())
                } catch (_: IllegalArgumentException) {
                    ERROR
                }
            }
            return ERROR
        }
    }
}

fun SnackbarType.toModel(): SnackbarAttr{
    return when (this) {
        SnackbarType.ERROR -> SnackbarAttr(
            backgroundColor = MajorColor.SnackbarMessage.Error.background,
            image = Icons.Filled.Info,
            iconModifier = Modifier
                .rotate(MajorDimens.Snackbar.ICON_ROTATION)
                .size(MajorDimens.Snackbar.iconSize),
            tint = MajorColor.SnackbarMessage.Error.text,
        )
        SnackbarType.VALIDATION -> SnackbarAttr(
            backgroundColor = MajorColor.SnackbarMessage.Validation.background,
            image = Icons.Filled.Info,
            iconModifier = Modifier
                .rotate(MajorDimens.Snackbar.ICON_ROTATION)
                .size(MajorDimens.Snackbar.iconSize),
            tint = MajorColor.SnackbarMessage.Validation.text,
        )
    }
}
