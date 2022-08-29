buildscript {
    repositories {
        google()
        mavenCentral()
    }
    dependencies {
        classpath(AppDependencies.gradleClasspath)
        classpath(AppDependencies.kotlinClasspath)
        classpath(AppDependencies.hiltClasspath)
    }
}

allprojects {
    repositories {
        google()
        mavenCentral()
        maven(url = "https://jitpack.io")
    }
}

tasks.withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
    kotlinOptions {
        jvmTarget = "11"
    }
}

tasks.register("clean", Delete::class) {
    delete(rootProject.buildDir)
}
