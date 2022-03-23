package com.hyyu.votesimulation.ui.main.adapter

import android.view.LayoutInflater
import android.view.ViewGroup
import androidx.recyclerview.widget.RecyclerView
import com.hyyu.votesimulation.R
import com.hyyu.votesimulation.model.Blog
import com.hyyu.votesimulation.ui.main.ElectionViewHolder

class ElectionsAdapter(private val listener: ElectionItemListener): RecyclerView.Adapter<ElectionViewHolder>() {

    companion object {
        val TAG: String = ElectionsAdapter::class.java.simpleName
    }

    interface ElectionItemListener {
        fun onClickedElection(blogTitle: CharSequence)
    }

    private val items = ArrayList<Blog>()
    private lateinit var currentElection: Blog

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ElectionViewHolder {
        val view = LayoutInflater.from(parent.context).inflate(R.layout.item_election, parent, false)
        return ElectionViewHolder(view, listener)
    }

    override fun getItemCount(): Int = items.size

    override fun onBindViewHolder(holder: ElectionViewHolder, position: Int) {

    }
}
