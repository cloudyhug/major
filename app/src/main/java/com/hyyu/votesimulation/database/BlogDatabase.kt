package com.hyyu.votesimulation.database

import androidx.room.Database
import androidx.room.RoomDatabase

@Database(entities = [BlogCacheEntity::class], version = 1, exportSchema = false)
abstract class BlogDatabase : RoomDatabase() {
    abstract fun blogDao(): BlogDao

    companion object {
        const val DATABASE_NAME: String = "blog_db"
    }
}