package com.hyyu.votesimulation.ui.elections

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.activity.viewModels
import androidx.recyclerview.widget.DividerItemDecoration
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import com.hyyu.votesimulation.R
import com.hyyu.votesimulation.databinding.ActivityElectionsBinding

class ElectionsActivity : AppCompatActivity() {

  companion object {
    val TAG: String = ElectionsActivity::class.java.simpleName
  }

  val viewModel: ElectionsViewModel by viewModels()

  private lateinit var binding: ActivityElectionsBinding

  private var electionList: ArrayList<ElectionInfo> = emptyArray<>()

  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
    binding = ActivityElectionsBinding.inflate(layoutInflater)
    setContentView(binding.root)

    fillElections()

    binding.rvElections.layoutManager = LinearLayoutManager(this)
    binding.rvElections.addItemDecoration(
      DividerItemDecoration(this, LinearLayoutManager.VERTICAL))
    binding.rvElections.adapter = ElectionAdapter(electionList)
  }

  private fun fillElections() {
    viewModel.fillElections(electionList)
  }
}

data class ElectionInfo(val id: Int, val name: String, val isRunning: Boolean)

class ElectionAdapter(
  private val electionList: List<ElectionInfo>
) : RecyclerView.Adapter<ElectionViewHolder>() {
  override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ElectionViewHolder {
    val itemView = LayoutInflater.from(parent.context).inflate(R.layout.election_list_row, parent, false)
    return ElectionViewHolder(itemView)
  }

  override fun onBindViewHolder(holder: ElectionViewHolder, position: Int) {
    val electionInfo = electionList[position]
    holder.name.text = electionInfo.name
  }

  override fun getItemCount() = electionList.size

}

class ElectionViewHolder(itemView: View) : RecyclerView.ViewHolder(itemView) {
  val name = itemView.findViewById<TextView>(R.id.name)
}

  /* Observer on the dataSate liveData  */
  /* private fun subscribeObservers() {
    viewModel.connectionDataState.observe(this) { dataState ->
      when (dataState) {
        is DataState.Success<ConnectionObjectResponse> -> {
          binding.btnSend.revertAnimation()
          startActivity(Intent(this, MainActivity::class.java))
          finish()
        }
        is DataState.Loading -> {
          if (registerDialog?.showsDialog != true)
            binding.btnSend.startAnimation()
        }
        is DataState.Error -> {
          binding.btnSend.revertAnimation()
          if (registerDialog?.showsDialog == true) {
            displayErrorOnSnackbar(registerDialog?.getRootLayout()!!, dataState.exception.message)
          } else {
            displayErrorOnSnackbar(binding.root, dataState.exception.message)
          }
        }
      }
    } */
