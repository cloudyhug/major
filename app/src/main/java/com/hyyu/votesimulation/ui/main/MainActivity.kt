package com.hyyu.votesimulation.ui.main

import androidx.appcompat.app.AppCompatActivity
import androidx.compose.runtime.Composable
import android.os.Bundle
import android.util.Log
import androidx.activity.compose.setContent
import androidx.activity.viewModels
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.hyyu.votesimulation.ui.theme.MajorTheme
import com.hyyu.votesimulation.ui.theme.Orange700
import dagger.hilt.android.AndroidEntryPoint

@AndroidEntryPoint
class MainActivity : AppCompatActivity() {

    companion object {
        val TAG: String = MainActivity::class.java.simpleName
    }

    val viewModel: MainViewModel by viewModels()

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        viewModel.launchInitialCalls()

        setupObservers()
    }

    private fun initActivity() {
        Log.v(TAG, "Mock succeeded")
        // TODO: Network calls have been made, begin MainActivity startup here
        setContent {
            MajorTheme {
                ActivityScaffold()
            }
        }
    }

    private fun setupObservers() {
        viewModel.initDataState.observe(this) {
            initActivity()
        }
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
            Text(text = "Your history", fontSize = 16.sp, fontWeight = FontWeight.W700)
            Surface(color = MaterialTheme.colors.background) {
                LazyColumn(modifier = Modifier.padding(vertical = 4.dp)) {
                    items(items = viewModel.electionsList) { election ->
                        ElectionCard(name = election)
                    }
                }
            }
        }
    )

    @Composable
    fun ElectionCard(name: String) {
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
