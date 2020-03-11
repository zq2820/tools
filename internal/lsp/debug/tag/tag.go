// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package tag provides the labels used for telemetry throughout gopls.
package tag

import (
	"golang.org/x/tools/internal/telemetry/event"
	"golang.org/x/tools/internal/telemetry/stats"
	"golang.org/x/tools/internal/telemetry/unit"
)

var (
	// create the tag keys we use
	Method        = &event.Key{Name: "method"}
	StatusCode    = &event.Key{Name: "status.code"}
	StatusMessage = &event.Key{Name: "status.message"}
	RPCID         = &event.Key{Name: "id"}
	RPCDirection  = &event.Key{Name: "direction"}
	File          = &event.Key{Name: "file"}
	Directory     = &event.Key{Name: "directory"}
	URI           = &event.Key{Name: "URI"}
	Package       = &event.Key{Name: "package"}
	PackagePath   = &event.Key{Name: "package_path"}
	Query         = &event.Key{Name: "query"}
	Snapshot      = &event.Key{Name: "snapshot"}
	Operation     = &event.Key{Name: "operation"}

	Position     = &event.Key{Name: "position"}
	Category     = &event.Key{Name: "category"}
	PackageCount = &event.Key{Name: "packages"}
	Files        = &event.Key{Name: "files"}
	Port         = &event.Key{Name: "port"}
	Type         = &event.Key{Name: "type"}
	HoverKind    = &event.Key{Name: "hoverkind"}
)

var (
	// create the stats we measure
	Started       = stats.Int64("started", "Count of started RPCs.", unit.Dimensionless)
	ReceivedBytes = stats.Int64("received_bytes", "Bytes received.", unit.Bytes)
	SentBytes     = stats.Int64("sent_bytes", "Bytes sent.", unit.Bytes)
	Latency       = stats.Float64("latency_ms", "Elapsed time in milliseconds", unit.Milliseconds)
)

const (
	Inbound  = "in"
	Outbound = "out"
)