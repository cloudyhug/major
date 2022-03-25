package com.hyyu.votesimulation.ui.main

import androidx.lifecycle.*
import com.hyyu.votesimulation.repository.MainRepository
import com.hyyu.votesimulation.util.state.DataState
import dagger.hilt.android.lifecycle.HiltViewModel
import kotlinx.coroutines.Dispatchers
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

    fun launchInitialCalls() {
        viewModelScope.launch {
            withContext(Dispatchers.IO) {
                _initDataState.postValue(DataState.Success(Unit))
/*
                mainRepository.refreshAccessToken()
                    .onEach {
                        _initDataState.postValue(it)
                    }
                    .launchIn(viewModelScope)
*/
            }
        }
    }

}
