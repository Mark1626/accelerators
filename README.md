# Accelerators

This project is to be used within chipyard

## Configuring in Chipyard

**Cloning or submodule within chipyard**

```
cd chipyard/generators
git clone <this-repo>
```

**Adding to chipyard dependency**

```
// build.sbt
lazy val chipyard = (project in file("generators/chipyard"))
  .dependsOn(<other_deps>, // Replace with other deps at chipyard
    accelerators // Add this project
   )
  .settings(libraryDependencies ++= rocketLibDeps.value)
  .settings(commonSettings)
 
 lazy val accelerators = (project in file("./generators/accelerators"))
  .dependsOn(rocketchip, testchipip, dsptools, `rocket-dsp-utils`)
  .settings(libraryDependencies ++= rocketLibDeps.value)
  .settings(commonSettings)
```
