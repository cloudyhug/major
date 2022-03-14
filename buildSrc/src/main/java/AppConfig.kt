object AppConfig {
    const val applicationId = "com.hyyu.major"
    const val compileSdk = 32
    const val minSdk = 26
    const val targetSdk = 32

    const val androidTestInstrumentation = "androidx.test.runner.AndroidJUnitRunner"

    fun generateVersionCode(): Int = minSdk * 10000000 + Versions.versionMajor * 10000 + Versions.versionMinor * 100 + Versions.versionPatch
    fun generateVersionName(): String = "${Versions.versionMajor}.${Versions.versionMinor}.${Versions.versionPatch}"

}
