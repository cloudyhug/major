package com.hyyu.votesimulation.model

import com.google.gson.TypeAdapter
import com.google.gson.stream.JsonReader
import com.google.gson.stream.JsonWriter

enum class Rating {
  TERRIBLE, BAD, INADEQUATE, PASSABLE, SUFFICIENT, GOOD, EXCELLENT
}

class RatingAdapter: TypeAdapter<Rating>() {
  override fun write(writer: JsonWriter, value: Rating) { writer.value(value.ordinal) }
  override fun read(reader: JsonReader) = Rating.values()[reader.nextInt()]
}