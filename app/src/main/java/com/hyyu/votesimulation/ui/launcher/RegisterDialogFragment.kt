package com.hyyu.votesimulation.ui.launcher

import android.os.Build
import android.os.Bundle
import android.util.Log
import android.util.TypedValue
import android.view.*
import androidx.fragment.app.DialogFragment
import com.google.android.material.textfield.TextInputLayout
import com.hyyu.votesimulation.R
import com.hyyu.votesimulation.databinding.DialogRegisterBinding
import com.hyyu.votesimulation.util.const.AnimationConst
import com.hyyu.votesimulation.util.extension.*
import com.hyyu.votesimulation.util.handler.SimpleHandler

class RegisterDialogFragment : DialogFragment() {

    companion object {
        val TAG: String = RegisterDialogFragment::class.java.simpleName
    }

    private lateinit var binding: DialogRegisterBinding

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setStyle(STYLE_NORMAL, R.style.FullScreenDialogStyle)
    }

    override fun onStart() {
        super.onStart()

        dialog?.apply {
            val width = ViewGroup.LayoutParams.MATCH_PARENT
            val height = ViewGroup.LayoutParams.MATCH_PARENT

            window?.apply {
                attributes.windowAnimations = R.style.DialogAnimation
                addFlags(WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS)
                statusBarColor =
                    getAttributeFromTheme(com.google.android.material.R.attr.colorPrimaryVariant)
                setLayout(width, height)
            }
        }
    }

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View {
        binding = DialogRegisterBinding.inflate(requireActivity().layoutInflater)

        return binding.root
    }

    override fun onViewCreated(view: View, savedInstanceState: Bundle?) {
        super.onViewCreated(view, savedInstanceState)

        setupImeKeyboardParams()

        binding.toolbar.setNavigationOnClickListener {
            dismiss()
        }

        binding.btnSend.setOnClickListener {
            it.bounce {
                hideImeKeyboard(requireActivity(), binding.root.windowToken)
                clearErrorsOnInputs()

                val vm = (requireActivity() as LauncherActivity).viewModel
                val login = binding.editTiLogin.value
                val password = binding.editTiPassword.value

                vm.credentialsBody = vm.createCredentials(login, password)

                when (vm.validateCredentials(vm.credentialsBody)) {
                    LauncherViewModel.ValidatorCode.LOGIN_NOT_VALID -> displayErrorOnTIL(
                        binding.layoutTiLogin,
                        "Login must be valid"
                    )
                    LauncherViewModel.ValidatorCode.PASSWORD_NOT_VALID -> displayErrorOnTIL(
                        binding.layoutTiPassword,
                        "Password mustn't be empty"
                    )
                    else -> {
                        vm.setStateEvent(LauncherStateEvent.RegisterEvent)
                    }
                }
            }
        }
    }

    fun animLoadingButton() {
        binding.btnSend.startAnimation()
    }

    fun revertLoadingButton() {
        binding.btnSend.revertAnimation()
    }

    fun getRootLayout(): View = binding.root

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

    private fun setupImeKeyboardParams() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R)
            requireActivity().window?.setDecorFitsSystemWindows(false)
        else
            requireActivity().window?.setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_ADJUST_RESIZE)

        SimpleHandler.postDelayed({
            showImeKeyboardWithFocusOnView(requireActivity(), binding.editTiLogin)
        }, AnimationConst.HANDLER_DELAY_MEDIUM)
    }

    private fun getAttributeFromTheme(id: Int): Int {
        val typedValue = TypedValue()
        requireContext().theme.resolveAttribute(id, typedValue, true)
        return typedValue.data
    }

}
