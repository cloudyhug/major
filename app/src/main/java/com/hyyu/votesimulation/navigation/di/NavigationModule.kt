package com.hyyu.votesimulation.navigation.di

import com.hyyu.votesimulation.navigation.navigators.AppNavigator
import com.hyyu.votesimulation.navigation.navigators.AuthenticationNavigator
import com.hyyu.votesimulation.navigation.navigators.LauncherNavigator
import dagger.Module
import dagger.Provides
import dagger.hilt.InstallIn
import dagger.hilt.components.SingletonComponent
import javax.inject.Singleton

@Module
@InstallIn(SingletonComponent::class)
object NavigationModule {

    @Singleton
    @Provides
    fun providesAppNavigator() = AppNavigator()

    @Singleton
    @Provides
    fun providesLauncherNavigator() = LauncherNavigator()

    @Singleton
    @Provides
    fun providesAuthenticationNavigator() = AuthenticationNavigator()

}
