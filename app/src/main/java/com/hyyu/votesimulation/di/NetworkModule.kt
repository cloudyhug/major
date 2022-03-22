package com.hyyu.votesimulation.di

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import com.hyyu.votesimulation.model.Rating
import com.hyyu.votesimulation.model.RatingAdapter
import com.hyyu.votesimulation.network.MajorApi
import com.hyyu.votesimulation.prefs.Session
import dagger.Module
import dagger.Provides
import dagger.hilt.InstallIn
import dagger.hilt.components.SingletonComponent
import okhttp3.OkHttpClient
import okhttp3.logging.HttpLoggingInterceptor
import retrofit2.Retrofit
import retrofit2.converter.gson.GsonConverterFactory
import retrofit2.converter.scalars.ScalarsConverterFactory
import java.util.concurrent.TimeUnit
import javax.inject.Singleton

@Module
@InstallIn(SingletonComponent::class)
object NetworkModule {

    const val TIMEOUT = 10L

    @Singleton
    @Provides
    fun provideGsonBuilder(): Gson {
        return GsonBuilder()
            .excludeFieldsWithoutExposeAnnotation()
            .registerTypeAdapter(Rating::class.java, RatingAdapter())
            .create()
    }

    @Singleton
    @Provides
    fun provideRetrofit(sessionPrefs: Session, gson: Gson): Retrofit.Builder {
        val loggingInterceptor = HttpLoggingInterceptor()
        val clientBuilder = OkHttpClient.Builder().apply {
            connectTimeout(TIMEOUT, TimeUnit.SECONDS)
            writeTimeout(TIMEOUT, TimeUnit.SECONDS)
            readTimeout(TIMEOUT, TimeUnit.SECONDS)
            addNetworkInterceptor {
                var request = it.request()
                val newBuilder = request.newBuilder()

                newBuilder.addHeader("accessToken", sessionPrefs.accessToken ?: "")
                request = newBuilder.build()
                it.proceed(request)
            }
            loggingInterceptor.level = HttpLoggingInterceptor.Level.BODY
            addInterceptor(loggingInterceptor)
        }

        return Retrofit.Builder()
            .baseUrl(MajorApi.BASE_URL)
            .client(clientBuilder.build())
            .addConverterFactory(ScalarsConverterFactory.create())
            .addConverterFactory(GsonConverterFactory.create(gson))
    }

    @Singleton
    @Provides
    fun provideApi(retrofit: Retrofit.Builder): MajorApi {
        return retrofit
            .build()
            .create(MajorApi::class.java)
    }

}
