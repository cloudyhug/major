# Major Android App

## Description

Android client app allowing to take part in elections generated from the Major server.

## Project Structure

The gradle build is implemented with Kotlin DSL

The app is designed with MVVM architecture, using a repository with retrofit for the api management, a Room Database and Shared Preferences.

Dependency injection implemented with Hilt.

**Warning** The app currently uses HTTP instead of HTTPS.

## Usage

On first opening, register via the "No account? Register here" text under the login button.

Then, you'll gain access to the platform with elections you can take part in.