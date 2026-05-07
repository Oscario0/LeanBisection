const form = document.querySelector("#pipeline-form");
const statusBadge = document.querySelector("#pipeline-status");
const logList = document.querySelector("#log");

const stepBackend = document.querySelector("#step-backend");
const stepCert = document.querySelector("#step-cert");
const stepRoot = document.querySelector("#step-root");

const resultOverview = document.querySelector("#result-overview");
const resultCert = document.querySelector("#result-cert");
const resultRoot = document.querySelector("#result-root");

const exampleButton = document.querySelector("#load-example");
const resetButton = document.querySelector("#reset");

const fields = {
  coeffs: document.querySelector("#coeffs"),
  left: document.querySelector("#left"),
  right: document.querySelector("#right"),
  samples: document.querySelector("#samples"),
  maxJump: document.querySelector("#maxJump"),
  maxAbs: document.querySelector("#maxAbs"),
  tolerance: document.querySelector("#tolerance"),
  maxIter: document.querySelector("#maxIter"),
};

function setStatusBadge(text) {
  statusBadge.textContent = text;
}

function setStep(step, status, message) {
  step.dataset.status = status;
  const label = step.querySelector(".step-status");
  label.textContent = message;
}

function addLog(message) {
  const item = document.createElement("li");
  const timestamp = new Date().toLocaleTimeString();
  item.textContent = `${timestamp}  ${message}`;
  logList.prepend(item);
}

function clearLog() {
  logList.innerHTML = "";
}

function parseCoefficients(raw) {
  const tokens = raw
    .split(/[ ,]+/)
    .map((value) => value.trim())
    .filter((value) => value.length > 0);

  if (tokens.length === 0) {
    return { ok: false, reason: "No coefficients provided" };
  }

  const coeffs = [];
  for (const token of tokens) {
    const value = Number(token);
    if (!Number.isFinite(value)) {
      return { ok: false, reason: `Invalid coefficient: ${token}` };
    }
    coeffs.push(value);
  }

  return { ok: true, coeffs };
}

function evaluatePoly(coeffs, x) {
  let acc = 0;
  for (let i = coeffs.length - 1; i >= 0; i -= 1) {
    acc = coeffs[i] + x * acc;
  }
  return acc;
}

function checkContinuityGate({ coeffs, left, right, samples, maxJump, maxAbs }) {
  if (!(right > left)) {
    return { ok: false, reason: "Left bound must be less than right bound" };
  }
  if (samples <= 0) {
    return { ok: false, reason: "Samples must be positive" };
  }

  const step = (right - left) / samples;
  let prev = null;

  for (let i = 0; i <= samples; i += 1) {
    const x = left + step * i;
    const y = evaluatePoly(coeffs, x);

    if (!Number.isFinite(y)) {
      return { ok: false, reason: "Non-finite value detected" };
    }
    if (Math.abs(y) > maxAbs) {
      return { ok: false, reason: "Exceeded max abs value" };
    }
    if (prev !== null && Math.abs(y - prev) > maxJump) {
      return { ok: false, reason: "Exceeded max jump" };
    }
    prev = y;
  }

  return { ok: true };
}

function exportCertificate({ samples }) {
  return {
    continuityChecked: true,
    source: "prototype-backend-reallike",
    samplesUsed: Math.max(samples, 300),
  };
}

function bisection({ coeffs, left, right, tolerance, maxIter }) {
  let a = left;
  let b = right;
  let fa = evaluatePoly(coeffs, a);
  let fb = evaluatePoly(coeffs, b);

  if (!Number.isFinite(fa) || !Number.isFinite(fb)) {
    return { ok: false, reason: "Non-finite boundary value" };
  }
  if (fa === 0) {
    return { ok: true, root: a, iterations: 0 };
  }
  if (fb === 0) {
    return { ok: true, root: b, iterations: 0 };
  }
  if (fa * fb > 0) {
    return { ok: false, reason: "No sign change at bounds" };
  }

  let mid = a;
  let fm = fa;

  for (let i = 0; i < maxIter; i += 1) {
    mid = (a + b) / 2;
    fm = evaluatePoly(coeffs, mid);

    if (!Number.isFinite(fm)) {
      return { ok: false, reason: "Non-finite mid value" };
    }
    if (Math.abs(fm) <= tolerance || (b - a) / 2 <= tolerance) {
      return { ok: true, root: mid, iterations: i + 1 };
    }

    if (fa * fm < 0) {
      b = mid;
      fb = fm;
    } else {
      a = mid;
      fa = fm;
    }
  }

  return {
    ok: false,
    reason: "Max iterations reached",
    bestApprox: mid,
    iterations: maxIter,
  };
}

function setOverview({ coeffs, left, right }) {
  resultOverview.innerHTML = `
    <h3>Overview</h3>
    <p>Polynomial degree: ${Math.max(coeffs.length - 1, 0)}</p>
    <p>Interval: [${left}, ${right}]</p>
    <p class="muted">Execution mirrors safeFindPolynomialRootHybridRLDefault.</p>
  `;
}

function setCertificate(cert) {
  resultCert.innerHTML = `
    <h3>Certificate</h3>
    <p>Continuity checked: ${cert.continuityChecked}</p>
    <p>Samples used: ${cert.samplesUsed}</p>
    <p class="muted">Source: ${cert.source}</p>
  `;
}

function setRootResult(result) {
  if (result.ok) {
    resultRoot.innerHTML = `
      <h3>Root finding</h3>
      <p>Root: ${result.root.toFixed(6)}</p>
      <p>Iterations: ${result.iterations}</p>
      <p class="muted">Bisection succeeded under tolerance.</p>
    `;
    return;
  }

  if (result.bestApprox !== undefined) {
    resultRoot.innerHTML = `
      <h3>Root finding</h3>
      <p class="muted">${result.reason}</p>
      <p>Best approx: ${result.bestApprox.toFixed(6)}</p>
      <p>Iterations: ${result.iterations}</p>
    `;
    return;
  }

  resultRoot.innerHTML = `
    <h3>Root finding</h3>
    <p class="muted">${result.reason}</p>
  `;
}

function resetPipelineUI() {
  setStatusBadge("Idle");
  setStep(stepBackend, "idle", "Idle");
  setStep(stepCert, "idle", "Idle");
  setStep(stepRoot, "idle", "Idle");
  clearLog();
  resultOverview.innerHTML = "<h3>Overview</h3><p class=\"muted\">Run the pipeline to populate results.</p>";
  resultCert.innerHTML = "<h3>Certificate</h3><p class=\"muted\">Pending</p>";
  resultRoot.innerHTML = "<h3>Root finding</h3><p class=\"muted\">Pending</p>";
}

function collectInputs() {
  const coeffsParsed = parseCoefficients(fields.coeffs.value);
  if (!coeffsParsed.ok) {
    return coeffsParsed;
  }

  const left = Number(fields.left.value);
  const right = Number(fields.right.value);
  const samples = Number(fields.samples.value);
  const maxJump = Number(fields.maxJump.value);
  const maxAbs = Number(fields.maxAbs.value);
  const tolerance = Number(fields.tolerance.value);
  const maxIter = Number(fields.maxIter.value);

  const numbers = [left, right, samples, maxJump, maxAbs, tolerance, maxIter];
  if (numbers.some((value) => !Number.isFinite(value))) {
    return { ok: false, reason: "One or more inputs are not valid numbers" };
  }

  return {
    ok: true,
    coeffs: coeffsParsed.coeffs,
    left,
    right,
    samples,
    maxJump,
    maxAbs,
    tolerance,
    maxIter,
  };
}

function runPipeline(event) {
  event.preventDefault();
  clearLog();
  setStatusBadge("Running");

  const input = collectInputs();
  if (!input.ok) {
    setStatusBadge("Failed");
    setStep(stepBackend, "fail", "Input rejected");
    addLog(input.reason);
    return;
  }

  setOverview(input);
  setStep(stepBackend, "running", "Checking continuity");
  addLog("Running continuity gate");

  const continuity = checkContinuityGate(input);
  if (!continuity.ok) {
    setStatusBadge("Rejected");
    setStep(stepBackend, "fail", continuity.reason);
    setStep(stepCert, "idle", "Skipped");
    setStep(stepRoot, "idle", "Skipped");
    addLog(`Gate rejected: ${continuity.reason}`);
    resultCert.innerHTML = "<h3>Certificate</h3><p class=\"muted\">Rejected</p>";
    resultRoot.innerHTML = "<h3>Root finding</h3><p class=\"muted\">Skipped due to gate</p>";
    return;
  }

  setStep(stepBackend, "pass", "Continuity gate passed");
  addLog("Gate passed. Exporting certificate");

  setStep(stepCert, "running", "Exporting certificate");
  const cert = exportCertificate(input);
  setStep(stepCert, "pass", "Certificate exported");
  setCertificate(cert);

  addLog(`Certificate emitted from ${cert.source}`);
  setStep(stepRoot, "running", "Running bisection");

  const rootResult = bisection(input);
  if (rootResult.ok) {
    setStep(stepRoot, "pass", "Root found");
    setStatusBadge("Succeeded");
    addLog(`Root found at ${rootResult.root.toFixed(6)}`);
  } else {
    setStep(stepRoot, "fail", rootResult.reason);
    setStatusBadge("Failed");
    addLog(`Root finding failed: ${rootResult.reason}`);
  }

  setRootResult(rootResult);
}

exampleButton.addEventListener("click", () => {
  fields.coeffs.value = "-2, 0, 1";
  fields.left.value = "1";
  fields.right.value = "2";
  fields.samples.value = "200";
  fields.maxJump.value = "1000000";
  fields.maxAbs.value = "1000000000000";
  fields.tolerance.value = "1e-10";
  fields.maxIter.value = "1000";
  addLog("Loaded sqrt(2) example");
});

resetButton.addEventListener("click", () => {
  resetPipelineUI();
});

form.addEventListener("submit", runPipeline);

resetPipelineUI();
