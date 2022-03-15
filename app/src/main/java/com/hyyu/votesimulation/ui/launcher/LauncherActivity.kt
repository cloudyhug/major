package com.hyyu.votesimulation.ui.launcher

import android.content.Intent
import android.os.Bundle
import android.view.View
import androidx.activity.viewModels
import androidx.appcompat.app.AppCompatActivity
import androidx.fragment.app.FragmentTransaction
import androidx.fragment.app.FragmentTransaction.TRANSIT_FRAGMENT_MATCH_ACTIVITY_OPEN
import com.google.android.material.snackbar.Snackbar
import com.google.android.material.textfield.TextInputLayout
import com.hyyu.votesimulation.databinding.ActivityLauncherBinding
import com.hyyu.votesimulation.network.body.CredentialsObjectBody
import com.hyyu.votesimulation.network.response.ConnectionObjectResponse
import com.hyyu.votesimulation.network.response.RegisterObjectResponse
import com.hyyu.votesimulation.ui.main.MainActivity
import com.hyyu.votesimulation.util.const.AnimationConst
import com.hyyu.votesimulation.util.state.DataState
import com.hyyu.votesimulation.util.extension.bounce
import com.hyyu.votesimulation.util.extension.value
import com.hyyu.votesimulation.util.handler.SimpleHandler
import dagger.hilt.android.AndroidEntryPoint

@AndroidEntryPoint
class LauncherActivity : AppCompatActivity() {

    companion object {
        val TAG: String = LauncherActivity::class.java.simpleName
    }

    val viewModel: LauncherViewModel by viewModels()

    private lateinit var binding: ActivityLauncherBinding

    private var registerDialog: RegisterDialogFragment? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        binding = ActivityLauncherBinding.inflate(layoutInflater)
        setContentView(binding.root)

        setupInitialState()

        binding.btnSend.setOnClickListener {
            it.bounce {
                clearErrorsOnInputs()

                val login = binding.editTiLogin.value
                val password = binding.editTiPassword.value

                viewModel.credentialsBody = CredentialsObjectBody(login, password, "")

                when (viewModel.validateCredentials(viewModel.credentialsBody)) {
                    LauncherViewModel.ValidatorCode.LOGIN_NOT_VALID -> displayErrorOnTIL(binding.layoutTiLogin,"Login must be valid")
                    LauncherViewModel.ValidatorCode.PASSWORD_NOT_VALID -> displayErrorOnTIL(binding.layoutTiPassword, "Password mustn't be empty")
                    else -> {
                        viewModel.setStateEvent(LauncherStateEvent.ConnectEvent)
                    }
                }
            }
        }

        binding.tvNoAccount.setOnClickListener {
            it.bounce {
                openRegisterDialog()
            }
        }

    }

    private fun setupInitialState() {
        clearErrorsOnInputs()
        subscribeObservers()
    }

    /* Error management */

    private fun clearErrorsOnInputs() {
        binding.layoutTiLogin.isErrorEnabled = false
        binding.layoutTiPassword.isErrorEnabled = false
        binding.layoutTiLogin.error = null
        binding.layoutTiPassword.error = null
    }

    private fun displayErrorOnTIL(til: TextInputLayout, message: String) {
        til.error = message
        til.isErrorEnabled = true
    }

    private fun displayErrorOnSnackbar(v: View, message: String?) {
        Snackbar.make(v, message ?: "Unknown error", Snackbar.LENGTH_LONG).show()
    }

    /* Open the dialog to register a new account */

    private fun openRegisterDialog() {
        val ft: FragmentTransaction = supportFragmentManager.beginTransaction()
            .setTransition(TRANSIT_FRAGMENT_MATCH_ACTIVITY_OPEN)
        registerDialog = RegisterDialogFragment().apply {
            show(ft, RegisterDialogFragment.TAG)
        }
    }

    /* Observer on the dataSate liveData  */

    private fun subscribeObservers() {
        viewModel.connectionDataState.observe(this) { dataState ->
            when (dataState) {
                is DataState.Success<ConnectionObjectResponse> -> {
                    binding.btnSend.revertAnimation()
                    startActivity(Intent(this, MainActivity::class.java))
                    finish()
                }
                is DataState.Loading -> {
                    if (registerDialog?.showsDialog!!)
                    binding.btnSend.startAnimation()
                }
                is DataState.Error -> {
                    binding.btnSend.revertAnimation()
                    displayErrorOnSnackbar(binding.root, dataState.exception.message)
                }
            }
        }

        viewModel.registerDataState.observe(this) { dataState ->
            when (dataState) {
                is DataState.Success<RegisterObjectResponse> -> {
                    SimpleHandler.postDelayed({
                        viewModel.setStateEvent(LauncherStateEvent.ConnectEvent)
                    }, AnimationConst.HANDLER_DELAY_SHORT)
                }
                is DataState.Loading -> {
                    registerDialog?.animLoadingButton()
                }
                is DataState.Error -> {
                    runOnUiThread {
                        registerDialog?.revertLoadingButton()
                        displayErrorOnSnackbar(registerDialog!!.getRootLayout(), dataState.exception.message)
                    }
                }
            }
        }
    }

}
