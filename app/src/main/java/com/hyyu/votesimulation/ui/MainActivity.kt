package com.hyyu.votesimulation.ui

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.widget.Toast
import androidx.activity.viewModels
import com.hyyu.votesimulation.databinding.ActivityLauncherBinding
import com.hyyu.votesimulation.model.Blog
import com.hyyu.votesimulation.ui.launcher.LauncherViewModel
import dagger.hilt.android.AndroidEntryPoint
import kotlinx.coroutines.ExperimentalCoroutinesApi

@ExperimentalCoroutinesApi
@AndroidEntryPoint
class MainActivity : AppCompatActivity(), BlogAdapter.BlogItemListener {

    private lateinit var binding: ActivityLauncherBinding

    private val viewModel: LauncherViewModel by viewModels()

    private lateinit var adapter: BlogAdapter

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        binding = ActivityLauncherBinding.inflate(layoutInflater)
        setContentView(binding.root)

        setupRecyclerView()
        subscribeObservers()

        // viewModel.setStateEvent(LauncherStateEvent.GetBlogEvents)

/*
        binding.swipeRefreshLayout.setOnRefreshListener {
            viewModel.setStateEvent(MainStateEvent.GetBlogEvents)
        }
*/

    }

    private fun displayError(message: String?) {
        if (message.isNullOrEmpty()) {
            Toast.makeText(this, "Unknown error", Toast.LENGTH_LONG).show()
        } else {
            Toast.makeText(this, message, Toast.LENGTH_LONG).show()
        }
    }

    private fun displayLoading(isLoading: Boolean) {
/*
        binding.swipeRefreshLayout.isRefreshing = isLoading
*/
    }

    private fun populateRecyclerView(blogs: List<Blog>) {
        if (blogs.isNotEmpty()) adapter.setItems(ArrayList(blogs))
    }

    private fun setupRecyclerView() {
        adapter = BlogAdapter(this)
/*
        binding.blogRecyclerview.layoutManager = LinearLayoutManager(this)
        binding.blogRecyclerview.adapter = adapter
*/
    }

    override fun onClickedBlog(blogTitle: CharSequence) {
        Toast.makeText(this, blogTitle, Toast.LENGTH_SHORT).show()
    }

    private fun subscribeObservers() {
        /*
        viewModel.dataState.observe(this) { dataState ->
            when (dataState) {
                is DataState.Success<List<Blog>> -> {
                    displayLoading(false)
                    populateRecyclerView(dataState.data)
                }
                is DataState.Loading -> {
                    displayLoading(true)
                }
                is DataState.Error -> {
                    displayLoading(false)
                    displayError(dataState.exception.message)
                }
            }
        }
        */
    }

}
