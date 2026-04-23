# Incident response plan

## Triage

We monitor GitHub advisories, GitHub issues, the Gleam Discord, and emails to
security@gleam.run for security reports and bugs that may have security
implications.

If we spot a bug or report that looks like a security risk, we treat it as an
incident.

## Assessment

First we verify the accuracy of the report and evaluate its impact.

The security team of the Erlang Ecosystem Foundation may be contacted for
assistance.

## Response

Within 3 days of the report, we will acknowledge it, privately if the
vulnerability is sensitive. We follow a 90-day disclosure timeline.

A fix will be privately developed and backported to currently active versions
of the project.

## Communication

If the issue is confirmed as a vulnerability, we will open a GitHub Security
Advisory that will be published at the same time as the fix for the
vulnerability.

The advisory will detail the vulnerability and provide a way for users to
determine if they are impacted by the vulnerability.

## Recovery & Hardening

After fixing the issue, we review the circumstances that led to the
vulnerability and
evaluate what we could improve in our systems and processes to prevent similar
vulnerabilities in future.
