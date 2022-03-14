package com.hyyu.votesimulation.ui

import android.annotation.SuppressLint
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.appcompat.widget.AppCompatImageView
import androidx.constraintlayout.widget.ConstraintLayout
import androidx.recyclerview.widget.RecyclerView
import com.bumptech.glide.Glide
import com.bumptech.glide.request.RequestOptions
import com.hyyu.votesimulation.R
import com.hyyu.votesimulation.model.Blog

class BlogAdapter(private val listener: BlogItemListener) : RecyclerView.Adapter<BlogViewHolder>() {

    interface BlogItemListener {
        fun onClickedBlog(blogTitle: CharSequence)
    }

    private val items = ArrayList<Blog>()
    private lateinit var blog: Blog

    @SuppressLint("NotifyDataSetChanged")
    fun setItems(items: ArrayList<Blog>) {
        this.items.clear()
        this.items.addAll(items)
        notifyDataSetChanged()
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): BlogViewHolder {
        val view = LayoutInflater.from(parent.context).inflate(R.layout.item_blog, parent, false)
        return BlogViewHolder(view, listener)
    }

    override fun getItemCount(): Int = items.size


    override fun onBindViewHolder(holder: BlogViewHolder, position: Int) {
        blog = items[position]
        val blog = items[position]
        holder.textTitle.text = blog.title
        holder.textDescription.text = blog.body
        Glide.with(holder.itemLayout).load(blog.image)
            .placeholder(R.drawable.placeholder)
            .error(R.drawable.placeholder)
            .apply(RequestOptions().centerCrop())
            .into(holder.image)
    }
}

class BlogViewHolder(itemView: View, private val listener: BlogAdapter.BlogItemListener) :
    RecyclerView.ViewHolder(itemView),
    View.OnClickListener {

    val itemLayout: ConstraintLayout = itemView.findViewById(R.id.blog_layout)
    val textTitle: TextView = itemView.findViewById(R.id.text_title)
    val textDescription: TextView = itemView.findViewById(R.id.text_description)
    val image: AppCompatImageView = itemView.findViewById(R.id.image)

    init {
        itemLayout.setOnClickListener(this)
    }

    override fun onClick(v: View?) {
        listener.onClickedBlog(textTitle.text)
    }
}