object AppDependencies {

    /* Classpaths for top-level build script */
    const val gradleClasspath = "${Dependencies.gradle}:${Versions.gradle}"
    const val kotlinClasspath = "${Dependencies.kotlin}:${Versions.kotlin}"
    const val hiltClasspath = "${Dependencies.hilt}:${Versions.hilt}"

    /* Kotlin */
    private const val kotlinStdLib = "${Dependencies.kotlinStdLib}:${Versions.kotlin}"

    /* Kotlin Coroutines */
    private const val kotlinCoroutinesAndroid = "${Dependencies.kotlinCoroutinesAndroid}:${Versions.kotlinCoroutinesAndroid}"
    private const val kotlinCoroutinesCore = "${Dependencies.kotlinCoroutinesCore}:${Versions.kotlinCoroutinesCore}"

    /* Hilt */
    private const val hiltAndroid = "${Dependencies.hiltAndroid}:${Versions.hiltAndroid}"
    private const val hiltCompiler = "${Dependencies.hiltCompilerDagger}:${Versions.hiltCompilerDagger}"
    private const val hiltCompilerAndroid = "${Dependencies.hiltCompilerAndroid}:${Versions.hiltCompilerAndroid}"

    /* Retrofit */
    private const val retrofit = "${Dependencies.retrofit}:${Versions.retrofit}"
    private const val retrofitGson = "${Dependencies.retrofitGson}:${Versions.retrofitGson}"
    private const val loggingInterceptor = "${Dependencies.loggingInterceptor}:${Versions.loggingInterceptor}"
    private const val scalars = "${Dependencies.scalars}:${Versions.scalars}"

    /* Room */
    private const val roomCompiler = "${Dependencies.roomCompiler}:${Versions.roomCompiler}"
    private const val roomKtx = "${Dependencies.roomKtx}:${Versions.roomKtx}"
    private const val roomRuntime = "${Dependencies.roomRuntime}:${Versions.roomRuntime}"

    /* Preferences */
    private const val preference = "${Dependencies.preference}:${Versions.preference}"

    /* Android UI */
    private const val activityKtx = "${Dependencies.activtyKtx}:${Versions.activityKtx}"
    private const val appcompat = "${Dependencies.appcompat}:${Versions.appcompat}"
    private const val constraintLayout = "${Dependencies.constraintLayout}:${Versions.constraintLayout}"
    private const val coreKtx = "${Dependencies.coreKtx}:${Versions.coreKtx}"
    private const val fragmentKtx = "${Dependencies.fragmentKtx}:${Versions.fragmentKtx}"
    private const val material = "${Dependencies.material}:${Versions.material}"
    private const val swipeRefreshLayout = "${Dependencies.swipeRefreshLayout}:${Versions.swiperefreshLayout}"

    /* CircularProgressButton */
    private const val circularProgressButton = "${Dependencies.circularProgressButton}:${Versions.circularProgressButton}"

    /* Lifecycle */
    private const val lifeCycleExtensions = "${Dependencies.lifeCycleExtensions}:${Versions.lifeCycleExtensions}"
    private const val lifecycleLiveDataKtx = "${Dependencies.lifecycleLiveDataKtx}:${Versions.lifecycleLiveDataKtx}"
    private const val lifecycleViewModelKtx = "${Dependencies.lifecycleViewModelKtx}:${Versions.lifecycleViewModelKtx}"

    /* Glide */
    private const val glide = "${Dependencies.glide}:${Versions.glide}"
    private const val glideCompiler = "${Dependencies.glideCompiler}:${Versions.glideCompiler}"

    /* Tests */
    private const val jUnit = "${Dependencies.jUnit}:${Versions.jUnit}"
    private const val extJUnit = "${Dependencies.extJUnit}:${Versions.extJunit}"
    private const val espressoCore = "${Dependencies.espressoCore}:${Versions.espressoCore}"

    /* Hilt Testing */
    private const val hiltTestAndroid = "${Dependencies.hiltTestAndroid}:${Versions.hiltTestAndroid}"
    private const val hiltTestCompilerAndroid = "${Dependencies.hiltTestCompilerAndroid}:${Versions.hiltTestCompilerAndroid}"
    private const val hiltTestCompilerAndroidX = "${Dependencies.hiltTestCompilerAndroidX}:${Versions.hiltTestCompilerAndroidX}"

    val appLibraries = arrayListOf<String>().apply {
        add(activityKtx)
        add(appcompat)
        add(circularProgressButton)
        add(constraintLayout)
        add(coreKtx)
        add(fragmentKtx)
        add(glide)
        add(hiltAndroid)
        add(kotlinCoroutinesAndroid)
        add(kotlinCoroutinesCore)
        add(kotlinStdLib)
        add(lifeCycleExtensions)
        add(lifecycleLiveDataKtx)
        add(lifecycleViewModelKtx)
        add(loggingInterceptor)
        add(material)
        add(preference)
        add(retrofit)
        add(retrofitGson)
        add(roomKtx)
        add(roomRuntime)
        add(scalars)
        add(swipeRefreshLayout)
    }

    val kaptLibraries = arrayListOf<String>().apply {
        add(glideCompiler)
        add(hiltCompiler)
        add(hiltCompilerAndroid)
        add(roomCompiler)
    }

    val androidTestLibraries = arrayListOf<String>().apply {
        add(espressoCore)
        add(extJUnit)
        add(hiltTestAndroid)
    }

    val testLibraries = arrayListOf<String>().apply {
        add(jUnit)
        add(hiltTestAndroid)
    }

    val kaptAndroidTestLibraries = arrayListOf<String>().apply {
        add(hiltTestCompilerAndroid)
        add(hiltTestCompilerAndroidX)
    }

}
