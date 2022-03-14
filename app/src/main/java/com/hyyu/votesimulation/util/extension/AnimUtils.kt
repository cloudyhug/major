package com.hyyu.votesimulation.util.extension

import android.view.View
import android.view.ViewPropertyAnimator
import com.hyyu.votesimulation.util.const.AnimationConst

@Utils
fun ViewPropertyAnimator.scaleXY(value: Float) = scaleX(value).scaleY(value)!!

@Utils
fun View.bounce(onFinish: (() -> Unit)? = null) {
    if (!isAttachedToWindow) {
        return
    }
    animate().scaleXY(AnimationConst.MIN_SCALE).setDuration(AnimationConst.ANIMATION_DURATION).withEndAction {
        animate().scaleXY(AnimationConst.MAX_SCALE).setDuration(AnimationConst.ANIMATION_DURATION).withEndAction {
            onFinish?.invoke()
        }
    }
}
