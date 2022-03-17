package com.hyyu.votesimulation.util.extension

import android.content.Context
import android.os.IBinder
import android.util.Log
import android.view.View
import android.view.inputmethod.InputMethodManager

/**
 * Retrieves the soft keyboard to allow its manipulation
 */
@Utils
fun getSoftKeyboard(context: Context): InputMethodManager = context.getSystemService(Context.INPUT_METHOD_SERVICE) as InputMethodManager

/**
 * Retrieves the soft keyboard to allow its manipulation, and requests focus on the provided view
 */
@Utils
fun getSoftKeyboardWithFocusOnView(context: Context, view: View): InputMethodManager? = if (view.requestFocus()) context.getSystemService(
    Context.INPUT_METHOD_SERVICE) as InputMethodManager else null

/**
 * Displays the keyboard
 */
@Utils
fun showImeKeyboard(context: Context, view: View) {
    val imm = getSoftKeyboard(context)
    imm.showSoftInput(view, InputMethodManager.SHOW_IMPLICIT)
}

/**
 * Displays the keyboard
 */
@Utils
fun showImeKeyboardWithFocusOnView(context: Context, view: View) {
    val imm = getSoftKeyboardWithFocusOnView(context, view)
    imm?.showSoftInput(view, InputMethodManager.SHOW_IMPLICIT)
}

/**
 * Hides the keyboard
 *
 * @param binder: select your layout's root view and get its windowtoken
 */
@Utils
fun hideImeKeyboard(context: Context, binder: IBinder) {
    val imm = getSoftKeyboard(context)
    imm.hideSoftInputFromWindow(binder, 0)
}
