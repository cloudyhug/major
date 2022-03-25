package com.hyyu.votesimulation.ui.elections

import androidx.lifecycle.*
import com.hyyu.votesimulation.model.Election
import com.hyyu.votesimulation.repository.MainRepository
import com.hyyu.votesimulation.util.state.DataState
import dagger.hilt.android.lifecycle.HiltViewModel
import kotlinx.coroutines.flow.launchIn
import kotlinx.coroutines.flow.onEach
import javax.inject.Inject

@HiltViewModel
class ElectionsViewModel
@Inject constructor(
  private val mainRepository: MainRepository,
  private val savedStateHandle: SavedStateHandle
) : ViewModel() {

  companion object {
    val TAG: String = ElectionsViewModel::class.java.simpleName
  }

  /* LiveData associated to info about elections */
  private val _electionsDataState: MutableLiveData<DataState<List<Election>>> = MutableLiveData()
  val electionsDataState: LiveData<DataState<List<Election>>>
    get() = _electionsDataState

//  lateinit var credentialsBody: CredentialsObjectBody

//  enum class ValidatorCode {
//    OK, LOGIN_NOT_VALID, PASSWORD_NOT_VALID
//  }

//  fun setStateEvent(event: LauncherStateEvent) {
//    viewModelScope.launch {
//      when (event) {
//        is LauncherStateEvent.ConnectEvent -> withContext(Dispatchers.IO) { logInToUserAccount() }
//        is LauncherStateEvent.RegisterEvent -> withContext(Dispatchers.IO) { registerNewUserAccount() }
//      }
//    }
//  }

  private suspend fun getElections() {
    mainRepository.getElections()
      .onEach { dataState ->
        _electionsDataState.postValue(dataState)
      }
      .launchIn(viewModelScope)
  }
}

//sealed class LauncherStateEvent {
//  object ConnectEvent : LauncherStateEvent()
//  object RegisterEvent : LauncherStateEvent()
//}
