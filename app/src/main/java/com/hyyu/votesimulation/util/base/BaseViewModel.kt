package com.hyyu.votesimulation.util.base

import androidx.annotation.StringRes
import androidx.lifecycle.ViewModel
import com.hyyu.votesimulation.model.common.snackbar.MajorSnackbarData
import com.hyyu.votesimulation.model.common.snackbar.SnackbarType
import com.hyyu.votesimulation.model.common.snackbar.SnackbarType.ERROR
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow

open class BaseViewModel : ViewModel() {
    private val _snackbarState = MutableStateFlow<MajorSnackbarData?>(null)
    val snackbarState: StateFlow<MajorSnackbarData?> = _snackbarState

    protected fun showSnackBar(
        message: String,
        type: SnackbarType = ERROR
    ) {
        _snackbarState.value = MajorSnackbarData(message, type)
    }

    fun onSnackbarDisplayed() {
        _snackbarState.value = null
    }

}
