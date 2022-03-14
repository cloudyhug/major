package com.hyyu.votesimulation.prefs

import androidx.preference.PreferenceManager
import com.hyyu.votesimulation.MajorApp

class Session {

    companion object {
        private const val PREF_SESSION_DEVICE_NAME_TAG = "SESSION_DEVICE_NAME"
        private const val PREF_SESSION_REFRESH_TOKEN_TAG = "SESSION_REFRESH_TOKEN"
        private const val PREF_SESSION_ACCESS_TOKEN_TAG = "SESSION_ACCESS_TOKEN"
    }

    private val mPrefs = PreferenceManager.getDefaultSharedPreferences(MajorApp.instance)

    var deviceName: String?
        get() = mPrefs?.getString(PREF_SESSION_DEVICE_NAME_TAG, "")
        set(deviceName) {
            mPrefs.edit().putString(PREF_SESSION_DEVICE_NAME_TAG, deviceName)
                .apply()
        }

    var accessToken: String?
        get() = mPrefs?.getString(PREF_SESSION_ACCESS_TOKEN_TAG, null)
        set(token) {
            mPrefs.edit().putString(PREF_SESSION_ACCESS_TOKEN_TAG, token)
                .apply()
        }

    var refreshToken: String?
        get() = mPrefs?.getString(PREF_SESSION_REFRESH_TOKEN_TAG, null)
        set(token) {
            mPrefs.edit().putString(PREF_SESSION_REFRESH_TOKEN_TAG, token)
                .apply()
        }

}
