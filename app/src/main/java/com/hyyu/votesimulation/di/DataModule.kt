package com.hyyu.votesimulation.di

import android.content.Context
import androidx.room.Room
import com.hyyu.votesimulation.database.BlogDao
import com.hyyu.votesimulation.database.BlogDatabase
import dagger.Module
import dagger.Provides
import dagger.hilt.InstallIn
import dagger.hilt.android.qualifiers.ApplicationContext
import dagger.hilt.components.SingletonComponent
import javax.inject.Singleton

@InstallIn(SingletonComponent::class)
@Module
object DataModule {

    @Singleton
    @Provides
    fun provideBlogDb(@ApplicationContext context: Context): BlogDatabase {
        return Room.databaseBuilder(
            context, BlogDatabase::class.java,
            BlogDatabase.DATABASE_NAME
        )
            .fallbackToDestructiveMigration()
            .build()
    }

    @Singleton
    @Provides
    fun provideBlogDAO(blogDatabase: BlogDatabase): BlogDao {
        return blogDatabase.blogDao()
    }
}
