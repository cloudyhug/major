package com.hyyu.votesimulation.ui.main

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import com.hyyu.votesimulation.databinding.ActivityLauncherBinding
import com.hyyu.votesimulation.databinding.ActivityMainBinding
import com.hyyu.votesimulation.ui.BlogAdapter

class MainActivity : AppCompatActivity() {

    private lateinit var binding: ActivityMainBinding

    companion object {
        val TAG: String = MainActivity::class.java.simpleName
    }

    private lateinit var adapter: BlogAdapter

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        binding = ActivityMainBinding.inflate(layoutInflater)
        setContentView(binding.root)

        setupRecyclerView()

    }

    private fun setupRecyclerView() {
        adapter = BlogAdapter(this)
/*
        binding.blogRecyclerview.layoutManager = LinearLayoutManager(this)
        binding.blogRecyclerview.adapter = adapter
*/
    }

}
