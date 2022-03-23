package com.hyyu.votesimulation.ui.main

import androidx.lifecycle.SavedStateHandle
import androidx.lifecycle.ViewModel
import com.hyyu.votesimulation.repository.MainRepository
import dagger.hilt.android.lifecycle.HiltViewModel
import javax.inject.Inject

@HiltViewModel
class MainViewModel
    @Inject constructor(
        private val mainRepository: MainRepository,
        private val savedStateHandle: SavedStateHandle
    ): ViewModel() {

}
