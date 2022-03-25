package com.hyyu.votesimulation.di

import com.hyyu.votesimulation.database.BlogDao
import com.hyyu.votesimulation.database.CacheMapper
import com.hyyu.votesimulation.network.MajorApi
import com.hyyu.votesimulation.network.ElectionMapper
import com.hyyu.votesimulation.prefs.Session
import com.hyyu.votesimulation.repository.MainRepository
import dagger.Module
import dagger.Provides
import dagger.hilt.InstallIn
import dagger.hilt.components.SingletonComponent
import javax.inject.Singleton

@InstallIn(SingletonComponent::class)
@Module
object RepositoryModule {

    @Singleton
    @Provides
    fun provideSessionPrefs(): Session {
        return Session()
    }

    @Singleton
    @Provides
    fun provideMainRepository(
        blogDao: BlogDao,
        majorApi: MajorApi,
        sessionPrefs: Session,
        cacheMapper: CacheMapper,
        electionMapper: ElectionMapper
    ): MainRepository {
        return MainRepository(blogDao, sessionPrefs, majorApi, cacheMapper, electionMapper)
    }

}
