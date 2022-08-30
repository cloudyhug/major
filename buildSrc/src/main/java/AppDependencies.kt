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
    private const val hiltNavigation = "${Dependencies.hiltNavigation}:${Versions.hiltNavigation}"

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
    private const val activityKtx = "${Dependencies.activityKtx}:${Versions.activityKtx}"
    private const val appcompat = "${Dependencies.appcompat}:${Versions.appcompat}"
    private const val constraintLayout = "${Dependencies.constraintLayout}:${Versions.constraintLayout}"
    private const val coreKtx = "${Dependencies.coreKtx}:${Versions.coreKtx}"
    private const val fragmentKtx = "${Dependencies.fragmentKtx}:${Versions.fragmentKtx}"
    private const val material = "${Dependencies.material}:${Versions.material}"
    private const val swipeRefreshLayout = "${Dependencies.swipeRefreshLayout}:${Versions.swiperefreshLayout}"

    /* Jetpack Compose */
    private const val composeActivity = "${Dependencies.composeActivity}:${Versions.composeActivity}"
    private const val composeAnimation = "${Dependencies.composeAnimation}:${Versions.composeAnimation}"
    private const val composeMaterial = "${Dependencies.composeMaterial}:${Versions.composeMaterial}"
    private const val composeNavigation = "${Dependencies.composeNavigation}:${Versions.composeNavigation}"
    private const val composeTooling = "${Dependencies.composeTooling}:${Versions.composeTooling}"
    private const val composeViewModel = "${Dependencies.composeViewModel}:${Versions.composeViewModel}"

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
    private const val extJUnit = "${Dependencies.extJUnit}:${Versions.extJunit}"
    private const val espressoCore = "${Dependencies.espressoCore}:${Versions.espressoCore}"
    private const val jUnit = "${Dependencies.jUnit}:${Versions.jUnit}"
    private const val jUnitCompose = "${Dependencies.jUnitCompose}:${Versions.jUnitCompose}"

    /* Hilt Testing */
    private const val hiltTestAndroid = "${Dependencies.hiltTestAndroid}:${Versions.hiltTestAndroid}"
    private const val hiltTestCompilerAndroid = "${Dependencies.hiltTestCompilerAndroid}:${Versions.hiltTestCompilerAndroid}"

    val appLibraries = arrayListOf<String>().apply {
        add(activityKtx)
        add(appcompat)
        add(circularProgressButton)
        add(composeActivity)
        add(composeAnimation)
        add(composeMaterial)
        add(composeNavigation)
        add(composeTooling)
        add(composeViewModel)
        add(constraintLayout)
        add(coreKtx)
        add(fragmentKtx)
        add(glide)
        add(hiltAndroid)
        add(hiltNavigation)
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
        add(roomCompiler)
    }

    val androidTestLibraries = arrayListOf<String>().apply {
        add(espressoCore)
        add(extJUnit)
        add(hiltTestAndroid)
        add(jUnitCompose)
    }

    val testLibraries = arrayListOf<String>().apply {
        add(jUnit)
        add(hiltTestAndroid)
    }

    val kaptAndroidTestLibraries = arrayListOf<String>().apply {
        add(hiltTestCompilerAndroid)
    }

}
