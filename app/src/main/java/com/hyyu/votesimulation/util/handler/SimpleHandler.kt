package com.hyyu.votesimulation.util.handler

import android.os.Handler
import android.os.Looper

object SimpleHandler {
    private val _handler = Handler(Looper.getMainLooper())

    fun postDelayed(r: () -> Unit, delay: Long) {
        _handler.postDelayed(r, delay)
    }

}
