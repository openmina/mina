let Command = ../../Command/Base.dhall
let RunInToolchain = ../../Command/RunInToolchain.dhall
let Docker = ../../Command/Docker/Type.dhall
let Size = ../../Command/Size.dhall


let Prelude = ../../External/Prelude.dhall

let Cmd = ../../Lib/Cmds.dhall
let S = ../../Lib/SelectFiles.dhall
let D = S.PathPattern

let Pipeline = ../../Pipeline/Dsl.dhall
let PipelineTag = ../../Pipeline/Tag.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall

let Command = ../../Command/Base.dhall
let RunInToolchain = ../../Command/RunInToolchain.dhall
let Docker = ../../Command/Docker/Type.dhall
let Size = ../../Command/Size.dhall

let buildTestCmd : Size -> Command.Type = \(cmd_target : Size) ->
  Command.build
    Command.Config::{
      commands = RunInToolchain.runInToolchain ([] : List Text) "buildkite/scripts/terraform-test.sh",
      label = "terraform-network-tests",
      key = "terraform network test",
      target = cmd_target,
      docker = None Docker.Type
    }

in

Pipeline.build
  Pipeline.Config::{
    spec = 
      let unitDirtyWhen = [
        S.strictlyStart (S.contains "src/automation/terraform"),
        S.strictlyStart (S.contains "src/helm"),
        S.strictlyStart (S.contains "buildkite/src/Jobs/Test/TerraformNetworkTest"),
        S.strictlyStart (S.contains "buildkite/scripts/terraform-test.sh")
      ]

      in

      JobSpec::{
        dirtyWhen = unitDirtyWhen,
        path = "Test",
        name = "TerraformNetworkTest",
        tags = [ PipelineTag.Type.Fast, PipelineTag.Type.Test ]
      },
    steps = [
      buildTestCmd Size.Large
    ]
  }