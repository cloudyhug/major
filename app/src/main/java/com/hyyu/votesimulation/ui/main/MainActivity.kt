package com.hyyu.votesimulation.ui.main

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import androidx.activity.compose.setContent
import androidx.activity.viewModels
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.livedata.observeAsState
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.hyyu.votesimulation.model.Election
import com.hyyu.votesimulation.ui.main.state.MainStateEvent
import com.hyyu.votesimulation.ui.main.viewmodel.MainViewModel
import com.hyyu.votesimulation.ui.theme.MajorTheme
import com.hyyu.votesimulation.util.state.DataState
import dagger.hilt.android.AndroidEntryPoint

@AndroidEntryPoint
class MainActivity : AppCompatActivity() {

    companion object {
        val TAG: String = MainActivity::class.java.simpleName
    }

    val viewModel: MainViewModel by viewModels()

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            MajorTheme {
                ActivityScaffold()
            }
        }

        setupObservers()

        viewModel.setStateEvent(MainStateEvent.InitialNetworkCalls)
    }

    private fun initActivity() {
    }

    private fun setupObservers() {
        viewModel.initDataState.observe(this) { dataState ->
            when (dataState) {
                is DataState.Success -> initActivity()
                // is DataState.Error -> DisplayErrorOnSnackbar(dataState.exception.message ?: "Unknown message")
            }
        }
    }

    @Composable
    private fun DisplayErrorOnSnackbar(message: String) {
        val scope = rememberCoroutineScope()
        val snackbarHostState = remember { SnackbarHostState() }

        LaunchedEffect(snackbarHostState) {
            snackbarHostState.showSnackbar(message)
        }
        SnackbarHost(snackbarHostState)
    }

    @Composable
    fun ActivityScaffold() = Scaffold(
        topBar = {
            TopAppBar {
                Row(modifier = Modifier.padding(16.dp)) {
                    Text(text = "Home", fontWeight = FontWeight.W700)
                }
            }
        },
        content = {
            val itemList: List<Election> by viewModel.electionsList.observeAsState(listOf())

            Text(text = "Your history", fontSize = 16.sp, fontWeight = FontWeight.W700)
            Surface(color = MaterialTheme.colors.background) {
                LazyColumn(modifier = Modifier.padding(vertical = 4.dp)) {
                    items(items = itemList) { election ->
                        ElectionItem(name = election.name)
                    }
                }
            }
        }
    )

    @Composable
    fun ElectionItem(name: String) {
        Surface(
            modifier = Modifier.padding(16.dp),
            color = MaterialTheme.colors.primary
        ) {
            Row(modifier = Modifier
                .padding(horizontal = 24.dp)
                .padding(
                    top = 24.dp,
                    bottom = 24.dp
                )
                .fillMaxWidth()
            ) {
                Text(text = name)
            }
        }
    }

    @Preview(showBackground = true)
    @Composable
    fun DefaultPreview() {
        MajorTheme {
            ActivityScaffold()
        }
    }

}
