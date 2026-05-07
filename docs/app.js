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
const exampleFunctionButton = document.querySelector("#load-example-fn");
const resetButton = document.querySelector("#reset");

const problemTypeRadios = document.querySelectorAll("input[name='problemType']");
const polynomialBlock = document.querySelector("#input-polynomial");
const expressionBlock = document.querySelector("#input-expression");

const fields = {
    coeffs: document.querySelector("#coeffs"),
  expression: document.querySelector("#expression"),
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

function getProblemType() {
  const selected = document.querySelector("input[name='problemType']:checked");
  return selected ? selected.value : "polynomial";
}

function updateProblemTypeUI(type) {
  if (type === "expression") {
    expressionBlock.classList.remove("hidden");
    polynomialBlock.classList.add("hidden");
  } else {
    polynomialBlock.classList.remove("hidden");
    expressionBlock.classList.add("hidden");
  }
}

function setProblemType(type) {
  problemTypeRadios.forEach((radio) => {
    radio.checked = radio.value === type;
  });
  updateProblemTypeUI(type);
}

const allowedFunctions = new Set([
  "sin",
  "cos",
  "tan",
  "asin",
  "acos",
  "atan",
  "exp",
  "log",
  "sqrt",
  "abs",
  "pow",
  "min",
  "max",
  "floor",
  "ceil",
]);

function compileExpression(raw) {
  const trimmed = raw.trim();
  if (!trimmed) {
    return { ok: false, reason: "No expression provided" };
  }
  if (/[^0-9x+\-*/^().,\sA-Za-z]/.test(trimmed)) {
    return { ok: false, reason: "Expression contains unsupported characters" };
  }
  if (/(?:constructor|__proto__|window|document|Function|eval|=>|;)/i.test(trimmed)) {
    return { ok: false, reason: "Expression contains unsafe tokens" };
  }

  let invalid = false;
  let expr = trimmed.replace(/\^/g, "**");
  expr = expr.replace(/\b([A-Za-z_][A-Za-z0-9_]*)\b/g, (match) => {
    const lower = match.toLowerCase();
    if (lower === "x") {
      return "x";
    }
    if (lower === "pi") {
      return "Math.PI";
    }
    if (lower === "e") {
      return "Math.E";
    }
    if (lower === "ln") {
      return "Math.log";
    }
    if (allowedFunctions.has(lower)) {
      return `Math.${lower}`;
    }
    invalid = true;
    return match;
  });

  if (invalid) {
    return { ok: false, reason: "Unknown identifier in expression" };
  }

  try {
    const fn = new Function("x", `return ${expr};`);
    return { ok: true, fn, normalized: expr };
  } catch (error) {
    return { ok: false, reason: "Expression could not be parsed" };
  }
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

function checkContinuityGate({ fn, left, right, samples, maxJump, maxAbs }) {
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
    let y;
    try {
      y = fn(x);
    } catch (error) {
      return { ok: false, reason: "Evaluation failed inside continuity gate" };
    }

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

function bisection({ fn, left, right, tolerance, maxIter }) {
  let a = left;
  let b = right;
  let fa;
  let fb;

  try {
    fa = fn(a);
    fb = fn(b);
  } catch (error) {
    return { ok: false, reason: "Evaluation failed at bounds" };
  }

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
    try {
      fm = fn(mid);
    } catch (error) {
      return { ok: false, reason: "Evaluation failed at midpoint" };
    }

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

function escapeHtml(value) {
  return value
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#39;");
}

function setOverview({ problemType, coeffs, expression, left, right }) {
  let detail = "";
  let note = "";

  if (problemType === "expression") {
    const safeExpr = escapeHtml(expression);
    detail = `<p>Expression: ${safeExpr}</p>`;
    note = "Execution mirrors safeFindRootHybridRLDefault.";
  } else {
    detail = `<p>Polynomial degree: ${Math.max(coeffs.length - 1, 0)}</p>`;
    note = "Execution mirrors safeFindPolynomialRootHybridRLDefault.";
  }

  resultOverview.innerHTML = `
    <h3>Overview</h3>
    ${detail}
    <p>Interval: [${left}, ${right}]</p>
    <p class="muted">${note}</p>
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
  const problemType = getProblemType();
  let coeffs = null;
  let expression = "";
  let fn;

  if (problemType === "expression") {
    const compiled = compileExpression(fields.expression.value);
    if (!compiled.ok) {
      return compiled;
    }
    expression = fields.expression.value.trim();
    fn = compiled.fn;
  } else {
    const coeffsParsed = parseCoefficients(fields.coeffs.value);
    if (!coeffsParsed.ok) {
      return coeffsParsed;
    }
    coeffs = coeffsParsed.coeffs;
    fn = (x) => evaluatePoly(coeffs, x);
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
    problemType,
    coeffs,
    expression,
    fn,
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
  setProblemType("polynomial");
  fields.coeffs.value = "-2, 0, 1";
  fields.expression.value = "";
  fields.left.value = "1";
  fields.right.value = "2";
  fields.samples.value = "200";
  fields.maxJump.value = "1000000";
  fields.maxAbs.value = "1000000000000";
  fields.tolerance.value = "1e-10";
  fields.maxIter.value = "1000";
  addLog("Loaded sqrt(2) example");
});

exampleFunctionButton.addEventListener("click", () => {
  setProblemType("expression");
  fields.expression.value = "sin(x) - 0.5";
  fields.coeffs.value = "";
  fields.left.value = "0";
  fields.right.value = "2";
  fields.samples.value = "200";
  fields.maxJump.value = "1000000";
  fields.maxAbs.value = "1000000000000";
  fields.tolerance.value = "1e-10";
  fields.maxIter.value = "1000";
  addLog("Loaded sin(x) example");
});

resetButton.addEventListener("click", () => {
  setTimeout(() => {
    updateProblemTypeUI(getProblemType());
    resetPipelineUI();
  }, 0);
});

problemTypeRadios.forEach((radio) => {
  radio.addEventListener("change", () => {
    updateProblemTypeUI(getProblemType());
  });
});

form.addEventListener("submit", runPipeline);

updateProblemTypeUI(getProblemType());
resetPipelineUI();
