package com.hyyu.votesimulation.ui.elections

import androidx.lifecycle.*
import com.hyyu.votesimulation.repository.MainRepository
import dagger.hilt.android.lifecycle.HiltViewModel
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

//  /* LiveData associated to user's registering */
//  private val _registerDataState: MutableLiveData<DataState<Unit>> = MutableLiveData()
//  val registerDataState: LiveData<DataState<Unit>>
//    get() = _registerDataState

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

//  private suspend fun logInToUserAccount() {
//    mainRepository.logInToUserAccount(credentialsBody)
//      .onEach { dataState ->
//        _connectionDataState.postValue(dataState)
//      }
//      .launchIn(viewModelScope)
//  }
}

//sealed class LauncherStateEvent {
//  object ConnectEvent : LauncherStateEvent()
//  object RegisterEvent : LauncherStateEvent()
//}
