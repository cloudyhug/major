package com.hyyu.votesimulation.ui.main.viewmodel

import android.util.Log
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.lifecycle.*
import com.hyyu.votesimulation.model.Election
import com.hyyu.votesimulation.repository.MainRepository
import com.hyyu.votesimulation.ui.main.state.MainStateEvent
import com.hyyu.votesimulation.util.state.DataState
import dagger.hilt.android.lifecycle.HiltViewModel
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

    private var _electionsList: MutableLiveData<List<Election>> = MutableLiveData(listOf())
    val electionsList: LiveData<List<Election>>
        get() = _electionsList

    fun setStateEvent(event: MainStateEvent) {
        viewModelScope.launch {
            when (event) {
                is MainStateEvent.InitialNetworkCalls -> launchInitialCalls()
            }
        }
    }

    private suspend fun launchInitialCalls() {
        mainRepository.refreshAccessToken()
            .onEach {
                Log.v(TAG, "DataState from /refreshAccessToken: $it")
                mainRepository.getElections()
                    .onEach { electionsDataState ->
                        Log.v(TAG, "DataState from /elections: $electionsDataState")

                        when (electionsDataState) {
                            is DataState.Success -> _electionsList.postValue(electionsDataState.data as List<Election>)
                            is DataState.Error -> _initDataState.postValue(DataState.Error(electionsDataState.exception))
                            is DataState.Loading -> _initDataState.postValue(DataState.Loading)
                        }

                        _initDataState.postValue(DataState.Success(Unit))
                    }
                    .launchIn(viewModelScope)
            }
            .launchIn(viewModelScope)
    }

}
