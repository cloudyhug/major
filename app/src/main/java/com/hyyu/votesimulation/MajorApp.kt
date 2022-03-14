package com.hyyu.votesimulation

import android.app.Application
import dagger.hilt.android.HiltAndroidApp

@HiltAndroidApp
class MajorApp : Application() {

    companion object {
        lateinit var instance: MajorApp
            private set
    }

    init {
        instance = this
    }

}
