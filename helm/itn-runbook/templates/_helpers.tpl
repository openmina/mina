{{/*
Expand the name of the chart.
*/}}
{{- define "itn-runbook.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Create a default fully qualified app name.
We truncate at 63 chars because some Kubernetes name fields are limited to this (by the DNS naming spec).
If release name contains chart name it will be used as a full name.
*/}}
{{- define "itn-runbook.fullname" -}}
{{- if .Values.fullnameOverride }}
{{- .Values.fullnameOverride | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- $name := default .Chart.Name .Values.nameOverride }}
{{- if contains $name .Release.Name }}
{{- .Release.Name | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Create chart name and version as used by the chart label.
*/}}
{{- define "itn-runbook.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Common labels
*/}}
{{- define "itn-runbook.labels" -}}
helm.sh/chart: {{ include "itn-runbook.chart" . }}
{{ include "itn-runbook.selectorLabels" . }}
{{- if .Chart.AppVersion }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
{{- end }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- end }}

{{/*
Selector labels
*/}}
{{- define "itn-runbook.selectorLabels" -}}
app.kubernetes.io/name: {{ include "itn-runbook.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end }}

{{/*
Create the name of the service account to use
*/}}
{{- define "itn-runbook.serviceAccountName" -}}
{{- if .Values.serviceAccount.create }}
{{- default (include "itn-runbook.fullname" .) .Values.serviceAccount.name }}
{{- else }}
{{- default "default" .Values.serviceAccount.name }}
{{- end }}
{{- end }}

{{- define "itn-runbook.startNetworkName" }}
{{- "start-network" }}
{{- end }}

{{- define "itn-runbook.schedule" }}
{{- $hm := (regexReplaceAll ".* ([0-2][0-9]):([0-5][0-9]).*" (.date | toString) "${1} ${2}")}}
{{- $hm := (regexSplit " " $hm 2)}}
{{- (printf "%s %s/%d * * *" (index $hm 1) (index $hm 0) .everyHour) | quote }}
{{- end }}

{{- define "itn-runbook.restartNodeName" }}
{{- "restart-random-node" }}
{{- end }}

{{- define "itn-runbook.restartNodeSchedule" }}
{{- if (and .Values.restartRandomNode .Values.restartRandomNode.schedule) }}
{{- .Values.restartRandomNode.schedule }}
{{- else }}
{{- include "itn-runbook.schedule" (dict "date" (now | dateModify "+30m") "everyHour" 6) }}
{{- end }}
{{- end }}

{{- define "itn-runbook.runZkappName" }}
{{- "run-zkapp" }}
{{- end }}

{{- define "itn-runbook.runZkappSchedule" }}
{{- if (and .Values.runZkapp .Values.runZkapp.schedule) }}
{{- .Values.runZkapp.schedule }}
{{- else }}
{{- include "itn-runbook.schedule" (dict "date" (now | dateModify "+8h") "everyHour" 8) }}
{{- end }}
{{- end }}
