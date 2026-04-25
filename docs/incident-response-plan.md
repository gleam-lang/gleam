# Incident response plan

## Stages

### 1. Triage

We monitor GitHub advisories, GitHub issues, the Gleam Discord, and emails to
security@gleam.run for security reports and bugs that may have security
implications.

If we spot a bug or report that looks like a security risk, we treat it as an
incident.

### 2. Assessment

First we verify the accuracy of the report and evaluate its impact.

The security team of the Erlang Ecosystem Foundation may be contacted for
assistance.

### 3. Response

Within 3 days of the report, we will acknowledge it, privately if the
vulnerability is sensitive. We follow a 90-day disclosure timeline.

A fix will be privately developed and backported to currently active versions
of the project.

### 4. Communication

If the issue is confirmed as a vulnerability, we will open a GitHub Security
Advisory that will be published at the same time as the fix for the
vulnerability.

The advisory will detail the vulnerability and provide a way for users to
determine if they are impacted by the vulnerability.

### 5. Recovery & Hardening

After fixing the issue, we review the circumstances that led to the
vulnerability and
evaluate what we could improve in our systems and processes to prevent similar
vulnerabilities in future.

## Responder checklist

1. Acknowledge the vulnerability by replying to the reporter. (3 day deadline)
2. Add the [Erlang Ecosystem CNA team](https://cna.erlef.org/contact) to the
   GitHub vulnerability report as collaborators.
  - [`IngelaAndin`](https://github.com/IngelaAndin) – OTP Core Contributor
  - [`maennchen`](https://github.com/maennchen) – CISO, EEF
  - [`voltone`](https://github.com/voltone) – Bram Verburg – Security WG Chair
3. Test the vulnerability and have any discussion about the details in the report comments.
4. Prepare a fix in a **private** branch.
5. The EEF team fills in the details of the report.
6. Resolve the vulnerability by doing these steps concurrently:
  1. Merge fix.
  2. Publish CVE on GitHub.
  3. Release fixed version.
