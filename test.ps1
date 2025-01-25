#!/usr/bin/env pwsh

$jobs = (@(
        {
            $env:RUST_BACKTRACE
            cargo test --no-default-features --color always
        },
        {
            cargo test --all-features --color always
        }
    ) | ForEach-Object { Start-ThreadJob $_ })

$jobs | ForEach-Object { $_ | Receive-Job -Wait }

function ForkAndJoin {
    [CmdletBinding()]
    param (
        [Parameter(ValueFromPipeline = $true)]
        [scriptblock[]] $ScriptBlocks
    )

    begin {
        $items = [System.Collections.Generic.List[System.Management.Automation.Job]]::new()
    }

    process {
        $ScriptBlocks
        | ForEach-Object { Start-ThreadJob -ScriptBlock $_ }
        | ForEach-Object { $items.Add($_) }
    }

    end {
        $items | ForEach-Object { Receive-Job -Wait $_ }
    }
}
