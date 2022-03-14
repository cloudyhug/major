plugins {
    id("com.android.application")
    id("dagger.hilt.android.plugin")
    kotlin("android")
    kotlin("kapt")
}

repositories {
    google()
    mavenCentral()
}

android {
    compileSdk = AppConfig.compileSdk
    defaultConfig {
        applicationId = AppConfig.applicationId
        minSdk = AppConfig.minSdk
        targetSdk = AppConfig.targetSdk
        versionCode = AppConfig.generateVersionCode()
        versionName = AppConfig.generateVersionName()
        testInstrumentationRunner = AppConfig.androidTestInstrumentation
    }
    buildTypes {
        getByName("release") {
            isMinifyEnabled = false
            proguardFiles(
                getDefaultProguardFile("proguard-android-optimize.txt"),
                "proguard-rules.pro"
            )
        }
    }
    buildFeatures {
        viewBinding = true
    }
}

dependencies {
    implementation(AppDependencies.appLibraries)
    implementation("androidx.appcompat:appcompat:1.4.1")
    implementation("com.google.android.material:material:1.5.0")
    implementation("androidx.constraintlayout:constraintlayout:2.1.3")
    kapt(AppDependencies.kaptLibraries)
    testImplementation(AppDependencies.testLibraries)
    androidTestImplementation(AppDependencies.androidTestLibraries)
    kaptAndroidTest(AppDependencies.kaptAndroidTestLibraries)
}