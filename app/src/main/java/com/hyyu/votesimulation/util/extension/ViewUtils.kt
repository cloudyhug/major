package com.hyyu.votesimulation.util.extension

import android.util.Patterns
import android.widget.EditText
import com.google.android.material.textfield.TextInputEditText

inline val EditText.value: String get() = text.toString().trim()
inline val TextInputEditText.value: String get() = text.toString().trim()

/*
fun String.isValidEmail(): Boolean = Patterns.EMAIL_ADDRESS.matcher(this).matches()
*/
fun String.isValidLogin(): Boolean = true
