import Lake
open Lake DSL

package «rw» where
  -- add package configuration options here

lean_lib «Rw» where
  -- add library configuration options here

@[default_target]
lean_exe «rw» where
  root := `Main
