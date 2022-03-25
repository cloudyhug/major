package com.hyyu.votesimulation.ui.main.viewmodel

import android.util.Log
import androidx.lifecycle.*
import com.hyyu.votesimulation.repository.MainRepository
import com.hyyu.votesimulation.util.state.DataState
import dagger.hilt.android.lifecycle.HiltViewModel
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.launchIn
import kotlinx.coroutines.flow.onEach
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import javax.inject.Inject

@HiltViewModel
class MainViewModel
@Inject constructor(
    private val mainRepository: MainRepository,
    private val savedStateHandle: SavedStateHandle
) : ViewModel() {

    companion object {
        val TAG: String = MainViewModel::class.java.simpleName
    }

    private val _initDataState: MutableLiveData<DataState<Unit>> = MutableLiveData()
    val initDataState: LiveData<DataState<Unit>>
        get() = _initDataState

    private var _electionsList = arrayListOf(
        "Election 0",
        "Election 1",
        "Election 2",
        "Election 3",
        "Election 4",
        "Election 5"
    )

    val electionsList: List<String>
        get() = _electionsList

    fun launchInitialCalls() {
        viewModelScope.launch {
            withContext(Dispatchers.IO) {
                Log.v(TAG, "Start Mock")
                mockRefreshTokenCall()
/*
                // TODO: Uncomment once getting the new server executor
                mainRepository.refreshAccessToken()
                    .onEach {
                        _initDataState.postValue(it)
                    }
                    .launchIn(viewModelScope)
*/
            }
        }
    }

    // TODO: Remove once getting the new server executor
    private suspend fun mockRefreshTokenCall() {
        delay(1000)
        _initDataState.postValue(DataState.Success(Unit))
    }

}
