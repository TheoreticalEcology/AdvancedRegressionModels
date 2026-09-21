# Open Issues — Applied Regression Analysis for Empirical Research

*91 remaining · 8 Critical · 11 High · 58 Medium · 14 Low/Style*

*Each issue has a "Proposed solution:" field — fill in and share with Claude to implement fixes.*

## 1A — Getting Started

### High

**High · line 479**

The for-loop re-implementation of apply uses mean(iris\[,i\]) without na.rm=TRUE, while the apply call above uses mean, na.rm=TRUE. On the NA-containing modified iris the two produce different results, defeating the purpose of the comparison.

**Proposed solution:**

------------------------------------------------------------------------

### Medium

**Medium · line 370**

The apply NA question only works if the student ran the iris-modification code in the same session. Students starting the section fresh will see no NA. Needs an explicit dependency note.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · line 415**

Parenthetical "(5, resp. 4)" appears without sufficient context to clarify whether the 5-column or 4-column iris is intended.

**Proposed solution:**

------------------------------------------------------------------------

### Low / Style

**Style · project-wide**

Exercise heading inconsistency: Exercise blocks use either \#### Exercise or \#### Task as the heading — both forms appear in the book. The style guide in CLAUDE.md now specifies \#### Exercise as the canonical heading (with \#### Tasks for multi-part blocks), but existing chapters have not been audited for uniformity. Decide which heading to standardize on, then sweep all chapters.

**Proposed solution: Use Exercise throught, check the entire book! → FIXED** (swept 2A; confirmed no other Task headings project-wide)

------------------------------------------------------------------------

**Style · project-wide**

Callout fence-depth inconsistency: Most callouts use three colons (:::), but a handful of nested blocks use four colons (::::). This is technically valid Pandoc syntax but inconsistent. Grep for \^:::: across all .qmd files to decide whether the nesting is intentional and document the rule, or flatten it to the standard three-colon form.

**Proposed solution: it's not on purpose, flatten it! → NO CHANGE NEEDED** (grep confirmed no 4-colon fences exist in the project)

------------------------------------------------------------------------

## 2A — Linear Regression

### Critical

**Critical · line 54**

Statistical notation error: y = f(x) + 𝒩(0, σ) — a distribution cannot be added to a function. Correct: y = f(x) + ε where ε \~ N(0, σ²), or y \~ N(f(x), σ²). Note also that convention uses σ² (variance), not σ, inside the normal parameterization.

**Proposed solution: Change to** y = f(x) + ε where ε \~ N(0, σ²) → **FIXED**

------------------------------------------------------------------------

### High

**High · line 886**

predict(fit, newdata = X) — X is never defined anywhere in the chapter. Will throw an error if a student runs it.

**Proposed solution:**

------------------------------------------------------------------------

**High · line 96**

Manual p-value uses df = 100 - 3 (97), but lm() uses df = n − p where p counts only regression coefficients, giving df = 98. The comparison the text promises will show a discrepancy.

**Proposed solution: Change to** df = 100 - 2 (98). Add the following note as a tip box:\

Title: Model parameters and (residual) degrees of freedomn

Model parameters: for a simple linear regression, y=β0​+β1​x+ε, there are three unknowns: β0, β1 and σ2. So the model has 3 parameters and e.g. for the AIC (see later), we will count it as such.\
\
Additionally to the model parameters, there is the concept of the (residual) degrees of freedom (df) which is used to consider the model complexity in statistical tests on the model structure. The df count the number of independent data points. The residual df ask: after fitting the regression line, how many independent data points remain in the residuals? For the residuals, the residual sd σ2 doesn't matter, so by this calculation, the model only uses up 2 df and we have 98 df remaining.

→ **FIXED** (df corrected to 100-2; tip callout added)

------------------------------------------------------------------------

### Medium

**Medium · line 96**

pt(t_statistic, ..., lower.tail=FALSE) \* 2 only gives a valid two-sided p-value when t_statistic \> 0. For a negative slope the result exceeds 1. Should be 2 \* pt(abs(t_statistic), df=..., lower.tail=FALSE).

**Proposed solution:**

------------------------------------------------------------------------

**Medium · line 121**

"multiply with 1.96" — 1.96 is the large-sample normal quantile. For small n the t-quantile should be used. The CI formula (estimate ± 1.96 × SE) is also not stated explicitly.

**Proposed solution: Add the formula together with a small note that this is a large-sample approximation. → FIXED**

------------------------------------------------------------------------

**Medium · line 912**

"p-values… remain valid, regardless of the sample size" — too strong. LM p-values are exact only under normal residuals; for non-normal data they require large n. This statement is incorrect.

**Proposed solution: add: if residuals are normal. → FIXED**

------------------------------------------------------------------------

**Medium · lines 387–399**

aov(fit) called on a fitted lm object without explanation. The standard idiom is anova(fit); aov() is used here because TukeyHSD() requires it, but this is never explained, leaving students confused about when to use which.

**Proposed solution: Check that the project uses anova(fit) or car::Anova(fit) and aov() only in specifc cases where it is needed as input for another function → FIXED** (aov() confirmed valid here for TukeyHSD; added note explaining this)

------------------------------------------------------------------------

**Medium · line 489**

Interaction formula table assigns subscript a₁ to the interaction term while a₂, a₃ go to main effects — reversing the convention used throughout the chapter. Inconsistent labeling will mislead students.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · lines 1032–1037**

Both bootstrap functions close over fit and airquality from the global environment rather than accepting them as arguments — a poor pedagogical pattern for students learning to write their own functions.

**Proposed solution:**

------------------------------------------------------------------------

## 2B — ANOVA

### Medium

**Medium · line 36**

The section heading says "R² explained by each model component" but dividing the residual SS by total SS gives the unexplained fraction (1 − R²). The framing should distinguish explained vs. residual variance explicitly.

**Proposed solution: correct the wording → FIXED** (clarified that last code line is residual fraction, not an explained component)

------------------------------------------------------------------------

**Medium · line 222**

Incomplete sentence: "We can see this when a model with a separate mean per day:" — has no main predicate. Needs revision before the code block.

**Proposed solution: add: "is fit" → FIXED**

------------------------------------------------------------------------

## 2C — Random Effects

### Critical

**Critical · lines 549–551**

Callout claims "mod2 has one parameter more than mod3." The opposite is true: mod3 (school as fixed effect) has \~67 parameters; mod2 (random intercept) has \~5. The paragraph's closing sentence ("the number of parameters used to estimate the response are identical") then contradicts this opening claim. The entire callout needs rewriting.

**Proposed solution:**

------------------------------------------------------------------------

### High

**High · lines 262–265**

brms::brm(...) result is not assigned to any variable, but the next line calls summary(mod1) — which refers to the earlier lme4 model, not the brms model. Students will get wrong or misleading output.

**Proposed solution: assign a variable and update the summary call → FIXED** (assigned to brm_mod, updated summary call)

------------------------------------------------------------------------

**High · line 888**

Live lmer formula contains a double operator: (1\|Var12) + + (1\|Var23). The stray + is an editing artifact that may cause errors depending on R version.

**Proposed solution: correct this → NO CHANGE NEEDED** (double + + not found in file; may be a Unicode artifact or pre-existing fix)

------------------------------------------------------------------------

### Medium

**Medium · lines 276–279**

Code demonstrating how to switch REML/ML (REML = FALSE) is hidden with echo = F. The surrounding text says "In lme4, this is done via" — but nothing is shown. Students cannot see the syntax.

**Proposed solution: Change to eval = F → FIXED**

------------------------------------------------------------------------

**Medium · lines 667–671**

Two callouts on REML conflict: the tip callout recommends switching to ML for model selection; the caution callout says "use the likelihood of these models" without specifying whether to refit with ML. No worked example is provided.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · line 471**

residuals(mod1, re.form = NULL) — re.form is not a valid argument for residuals.merMod; it belongs to predict.merMod. May be silently ignored. The idiomatic call is simply residuals(mod1).

**Proposed solution: Ignore, this is correct**

------------------------------------------------------------------------

**Medium · lines 534–543**

mod1/mod2/mod3 reused in the degrees-of-freedom example block for completely different models than used throughout the chapter. Students skimming will be confused about which mod1 is being discussed.

**Proposed solution:**

------------------------------------------------------------------------

## 2D — Nonlinear Regression

### Critical

**Critical · lines 27–28**

"we are interested in the development of a reaction rate over time" — the Michaelis-Menten model describes rate as a function of substrate concentration (S), not time. The data variables are v (rate) and S (substrate). This directly contradicts the model being fitted.

**Proposed solution: OK, but still the sense of the equation is to understand the development over time? In any case, please rephrase so that it is currect. → FIXED** (rephrased: rate as function of substrate concentration S, not time)

------------------------------------------------------------------------

### Medium

**Medium · lines 18–19**

Polynomial display equation uses a tilde: y \~ a₀ + a₁·x + a₂·x² + .... The tilde is R formula syntax, not mathematical equality. The equation should use =.

**Proposed solution: ok use = → FIXED**

------------------------------------------------------------------------

**Medium · line 21**

Linearisation by transforming y presented as "always preferable" without noting that it changes the error structure (e.g., fitting log(y) implies multiplicative errors, a different model from NLS with log-normal errors).

**Proposed solution: change to usually preferable, but add a note that the error structure is changed by the transformation. → FIXED**

------------------------------------------------------------------------

**Medium · line 53**

nls described as finding "the MLE" without qualification. MLE = NLS only under i.i.d. normal errors. The identification should be stated conditionally.

**Proposed solution: add this qualification. → FIXED** (added: NLS = MLE under i.i.d. normal residuals)

------------------------------------------------------------------------

**Medium · line 86**

"the MLE curvature will often not be approximately multivariate normal" — the curvature (Hessian) is a matrix, not a distribution. What is meant is that the sampling distribution of the MLE will not be well approximated by a multivariate normal. Garbled phrasing.

**Proposed solution: correct phrasing → FIXED** ("MLE curvature" → "sampling distribution of the MLE")

------------------------------------------------------------------------

**Medium · line 192**

Sentence cut off mid-thought: "Note that random effects in this case are effectively random slopes, as there is no" — the explanation is missing entirely.

**Proposed solution: add "no obvious intercept in a nonlinear function" → FIXED**

------------------------------------------------------------------------

**Medium · lines 92–93**

nlsContourRSS is described as "visualizing the likelihood contour" — the function plots the RSS contour. The terms are equivalent under normal errors but the mismatch between text and code name will confuse students.

**Proposed solution: add a short verbal addition about the equivalence → FIXED** (rephrased to "RSS contour"; added explanation of equivalence under normal errors)

------------------------------------------------------------------------

### Low / Style

**Low · line 102**

nlsBootstrap(mm, niter = 200) — 200 bootstrap iterations is too few for reliable confidence intervals; no cautionary note is given. Consider adding a warning or bumping to ≥ 1000.

**Proposed solution: bump to 1000 → FIXED**

------------------------------------------------------------------------

## 3A — Missing Data

### Medium

**Medium · line 108**

"The p-values are smaller now, as one would expect, having more data." For single imputation, smaller p-values are an artefact of artificially reduced standard errors — not legitimate data gain. The next sentence acknowledges the problem, but the initial framing teaches the wrong intuition first.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · line 90**

MAR defined as missingness not depending on X "but possibly on other variables" — should specify "other observed variables." Dependence on unobserved variables is MNAR, not MAR. This is a precise statistical distinction.

**Proposed solution: add this → FIXED** (added "other observed variables" to MAR definition)

------------------------------------------------------------------------

**Medium · lines 125–131**

The mitools/smi code block at the chapter's end has no surrounding explanation — no description of what the smi dataset is, what its variables represent, or what the example demonstrates. Reads as an orphaned fragment.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · lines 75–80**

airquality = airquality\[rows, \] — overwrites the airquality object in-place without comment. Undisclosed side effects in teaching code are a poor habit to model. Standard approach: complete.cases() or na.omit().

**Proposed solution:**

------------------------------------------------------------------------

**Medium · lines 113–114**

No set.seed() immediately before the stochastic multiple-imputation loop (replicate(20, missRanger(..., pmm.k=5))). The global set.seed(42) in an earlier chunk does not guarantee reproducibility across sessions.

**Proposed solution: add a seed → FIXED** (added set.seed(42) before replicate call)

------------------------------------------------------------------------

### Low / Style

**Low · line 90**

"possibly from other values in the dataset" — "depends on" requires "on", not "from".

**Proposed solution: correct this → FIXED** ("from other values" → "on other observed values" in MNAR)

------------------------------------------------------------------------

## 3B — Causal Inference

### Critical

**Critical · lines 171–173**

"if you include it, it will nearly act as a collider" (about Month) — incorrect. Month is a causal ancestor of Temp, Solar.R, etc., not a collider. A collider requires arrows pointing into it from both sides of the path under analysis. This actively mis-teaches the collider concept students just learned.

**Proposed solution:**

------------------------------------------------------------------------

### Medium

**Medium · line 31**

"d-connected, which is just a fancy word for correlated" — d-connection is a graph-theoretic property about whether an association exists given a conditioning set. Conflating it with correlation conflates a property of the causal graph with a property of the data.

**Proposed solution: if you agree, change to "fancy word for a causal link" → FIXED** (rephrased to accurate technical description of d-connection)

------------------------------------------------------------------------

**Medium · lines 185–248**

DAG specifies cover \~ age + elev + abiotic (no hetero), but the psem model includes hetero in the cover equation. The discrepancy between the graphical model shown and the model actually fitted is not discussed.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · line 177**

Raw URL dropped as a bare line in body text with no surrounding sentence or hyperlink formatting. Looks like an unfiled authoring note that was never integrated.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · lines 88–90**

abline(fit2, col="red") on a multiple regression silently uses only the first predictor's slope (Coffee). Students accustomed to simple regression may not realize the red line ignores the second predictor; no explanation is given.

**Proposed solution:**

------------------------------------------------------------------------

## 3C — Model Selection

### Critical

**Critical · line 209**

Shrinkage direction stated backwards: "For higher variances they are stronger biased to 0." In ridge/random-effects parameterization, higher variance = less shrinkage, not more. This is the central conceptual point of the section.

**Proposed solution: correct this → FIXED** (shrinkage direction corrected: higher variance = less shrinkage)

------------------------------------------------------------------------

### High

**High · lines 204–207**

ranef(ridgeGlmmTMB1)\[\[1\]\]\$g — for glmmTMB, \[\[1\]\] yields $cond; but the grouping variable is named "group", not "g". Returns NULL silently. Correct accessor: ranef(ridgeGlmmTMB1)$cond\$group. (The adaptive shrinkage section at line 256 uses the correct form.)

**Proposed solution:**

------------------------------------------------------------------------

**High · lines 466–470**

Exercise formula lm((x1 + x2 + x3)\^2) is missing the response variable and data= argument. Will throw an error. Should be something like lm(y \~ (x1 + x2 + x3)\^2, data = dat).

**Proposed solution:**

------------------------------------------------------------------------

### Medium

**Medium · lines 420–421**

"none of \[post-selection inference methods\] are readily available in R" — outdated. The selectiveInference package (Tibshirani et al.) implements post-selection inference for lasso and stepwise procedures and is available on CRAN.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · line 127**

"AIC is asymptotically identical to leave-one-out cross-validation" — stated without qualification. Holds only under specific regularity conditions (correct specification, large n, negative log-likelihood loss). Should add "under certain regularity conditions".

**Proposed solution:**

------------------------------------------------------------------------

**Medium · lines 96–97**

F-test described as "technically not exactly a likelihood-ratio test." For normal linear models, the F-test and LRT test the same null with equivalent power — the F distribution accounts for the estimated error variance the chi-squared ignores. Clearer: the F-test is the finite-sample version of the LRT for normal models.

**Proposed solution:**

------------------------------------------------------------------------

### Low / Style

**Low · lines 180, 212**

Numbered tip callout: both "Constant shrinkage" and "Adaptive shrinkage" are labelled "1." — the second should be "2."

**Proposed solution: Fix this**

------------------------------------------------------------------------

**Low · line 23**

Image filename BiasBari​anceTradeOff.jpg — "Bariance" should be "Variance". The file was likely saved with this typo and may need to be renamed on disk.

**Proposed solution:**

------------------------------------------------------------------------

## 4A — GLMs

### Critical

**Critical · lines 337, 360**

Gamma GLM section opens: "With the default log link, coefficients are interpreted multiplicatively." R's default for Gamma() is the inverse (canonical) link, not the log. The model is fit with explicit link="log". The callout box at line 360 correctly names the inverse link default — directly contradicting the opening sentence.

**Proposed solution:**

------------------------------------------------------------------------

### High

**High · lines 937–939**

cv.glm(..., cost = auc, K=5) — boot::cv.glm's cost argument expects a function with signature f(observed, predicted) returning a scalar. pROC::auc has a different signature and returns an "auc" object. Will error or give wrong results. A wrapper is needed: cost_auc \<- function(obs, pred) as.numeric(pROC::auc(obs, pred, quiet=TRUE)).

**Proposed solution:**

------------------------------------------------------------------------

**High · lines 959–974**

SDM mapping code uses deprecated raster, sp, and spplot packages, superseded by terra and sf. Should either be updated or clearly flagged as using legacy packages.

**Proposed solution:**

------------------------------------------------------------------------

### Medium

**Medium · line 663**

Conceptual GLMM formula contains link\^{-2} — should be link\^{-1} (the inverse link function).

**Proposed solution:**

------------------------------------------------------------------------

**Medium · lines 496–543**

Salamander analysis defines m1, m2, then jumps to m4 — m3 is never defined. Students following the code will be puzzled by the gap.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · line 229**

"anova() works for glm and glmmTMB, but not for lme4::glmer" — inaccurate. anova() works for glmer objects (it performs a LRT). What is unreliable is a sequential type-I ANOVA table from a single glmer model. The statement needs more precision.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · line 67**

Count data described as "(1, 2, 3)" — should start at 0. Zero counts are central to the zero-inflation discussion later in the same chapter.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · lines 120–129**

Deviance-based pseudo-R² formula attributed to "Cohen" — not a standard attribution. Cohen's well-known effect size is f² for linear models. The label should be removed or replaced with "deviance-based pseudo-R²".

**Proposed solution:**

------------------------------------------------------------------------

**Medium · lines 157–158**

"fit an lm with log-transformed counts, which is basically log link + normal distribution" — imprecise. Log-transforming y and fitting lm implies a log-normal model (multiplicative errors); a Poisson GLM with log link has a different variance structure. The distinction matters for model fit.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · line 243**

The cloglog link is called the "log-log link." Its standard name is the complementary log-log link (R function: cloglog).

**Proposed solution:**

------------------------------------------------------------------------

**Medium · line 682**

"there is no function to automatically create marginal predictions" — the marginaleffects package (widely used since \~2021) does exactly this.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · lines 305–310**

simulateResiduals(fit, plot = TRUE) followed by plot(res, quantreg = TRUE) — the first call already produces a plot; the second re-plots the same thing. One should be removed.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · lines 449–451**

plotResiduals(m1, form = ...) passes the raw model object. The already-computed DHARMa object res should be passed instead, to avoid redundant simulation and ensure consistency with the earlier diagnostic plots.

**Proposed solution:**

------------------------------------------------------------------------

### Low / Style

**Style · line 61**

Wikipedia attribution block in raw HTML: A {=html} fence at line 61 contains a Wikipedia copyright/attribution notice (citing the source of the logistic-sigmoid image). This was intentionally kept as raw HTML because it is legitimate attribution metadata, not a pedagogical element. Consider whether to convert it to a :::{.callout-note} so it renders consistently across output formats (e.g., PDF), or document in CLAUDE.md that this specific {=html} block is intentional and exempt from conversion.

**Proposed solution:**

------------------------------------------------------------------------

## 4B — Heteroskedasticity

### Medium

**Medium · lines 22–25**

Simulation produces true mean differences (A=7, B=2, C=4) but text says "all regression effects are not significant… suggesting no difference between groups." The test fails to detect real differences — a Type II error caused by heteroskedasticity. The framing hides the lesson.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · line 76**

"GLS = Generalized Least Squares" appears directly after a chapter on "GLMs = Generalized Linear Models." A brief clarifying note distinguishing the two would prevent confusion.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · lines 133–141**

m3 is assigned to an lm with sqrt-transformed response in the first solution block, then reassigned to a gls object in the second. If students run both blocks sequentially, the first result is silently overwritten. Use distinct names.

**Proposed solution:**

------------------------------------------------------------------------

### Low / Style

**Low · line 102**

"on top!" — exclamation mark and informal phrasing are inconsistent with academic register.

**Proposed solution:**

------------------------------------------------------------------------

## 4C — Correlation Structures

### Critical

**Critical · lines 45, 521–531**

corPagel implements Pagel's λ model (a scalar transformation of Brownian motion covariances), not the Ornstein-Uhlenbeck process. The callout box at line 45 and the main text at line 521 both misidentify it as OU. The OU process in nlme/ape is corMartins. These models have distinct biological interpretations.

**Proposed solution:**

------------------------------------------------------------------------

### High

**High · lines 649–651**

Multivariate GLM code block: lme4(abundance \~ ...) — lme4 is a package, not a function (should be lmer() or glmer()). Both calls in the block also have unclosed parentheses. The block is eval=F so the book renders, but students copying it will get syntax errors.

**Proposed solution:**

------------------------------------------------------------------------

### Medium

**Medium · lines 81, 87–93**

AR(1) formula x\_{t+1} = a·x_t + (1−a)·ε is non-standard. Canonical form: x\_{t+1} = a·x_t + ε. The non-standard form makes the noise variance depend on a, which is inconsistent with how corAR1 parameterizes the process.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · line 130**

testTemporalAutocorrelation(fit, time = 1:1000) — passes the raw lm object, but DHARMa requires simulateResiduals() output. Contrast with the correct usage at line 189 (testTemporalAutocorrelation(res, ...)).

**Proposed solution:**

------------------------------------------------------------------------

**Medium · lines 133–140**

Note callout inside the Temporal Correlation section refers to "a spatial misfit trend in time / space" — in the temporal section, spatial terminology is misleading. Should say "temporal trend / temporal correlation" throughout.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · line 147**

gls(obs\~1, corr = corAR1(...)) — the formal argument is correlation, not corr. Works via R's partial matching, but all other examples in the chapter correctly use correlation =. Teaches the wrong argument name.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · lines 21–25**

Bullet list says "Apart from random effects… common examples include: — Random effects (distance = group)." Random effects appear in both the "apart from" clause and as the first bullet item — a logical contradiction.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · line 657**

In the jSDM example, rr(Species + 0\|id, d=2) capitalizes Species while the rest of the formula uses lowercase species. R treats these as different variables and will error.

**Proposed solution:**

------------------------------------------------------------------------

### Low / Style

**Low · line 662**

"set this models in the general topic of correlations" — ungrammatical; should be "place these models within the general topic".

**Proposed solution:**

------------------------------------------------------------------------

**Low · line 459**

"You can also check with DHARMa, using this works also for GLMMs" — grammatically garbled; should be "You can also check with DHARMa; this also works for GLMMs."

**Proposed solution:**

------------------------------------------------------------------------

**Low · line 650**

Both calls in the jSDM code block have unclosed parentheses (see High issue above). Even in an eval=F block, closing them is good practice.

**Proposed solution:**

------------------------------------------------------------------------

## 5A — Summary

### Medium

**Medium · line 16**

Polynomial description reads a·x = b — uses = (an equation) instead of + (addition). Should be a·x + b or the standard β₀ + β₁x.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · line 131**

"try to control confounders, and if not, try to measure them" — in observational settings the order should be reversed: you measure confounders to adjust for them statistically; controlling is only possible in experiments. A key conceptual distinction for the target audience.

**Proposed solution:**

------------------------------------------------------------------------

### Low / Style

**Low · line 46**

"which allows all that, easier inclusion of shrinkage, and p-value / df problems less visible" — three non-parallel clauses; the sentence is grammatically incomplete. Needs restructuring.

**Proposed solution:**

------------------------------------------------------------------------

## 6C — Case Studies

### Critical

**Critical · lines 232–234**

Scouting Ants: base model glm(went.phero \~ ant_group_main, data = dat) uses the default Gaussian family for a binary (0/1) response. Should specify family = binomial. The solution block that follows silently switches to glmer(..., family="binomial") with no explanation of why the family changed.

**Proposed solution:**

------------------------------------------------------------------------

### High

**High · line 596**

PGLS polynomial: scale(Lnalt) \* I(scale(Lnalt)\^2) — the \* expands to main effects plus their interaction, adding an unintended cubic term. The surrounding text discusses only "the linear and quadratic effect." Should use + instead of \*.

**Proposed solution:**

------------------------------------------------------------------------

### Medium

**Medium · lines 596–611**

PGLS solution uses Lnote as response (untransformed), while the preceding analysis consistently uses log(Lnote). Models on different response scales are not directly comparable to each other or to the earlier lm.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · lines 212–217**

Rate ratios from the Poisson offset model described as "comparable to the paper's odds ratios." Rate ratios and odds ratios are different quantities; when the event is not rare, they can diverge substantially. Should note the distinction explicitly.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · lines 725–850**

SeedBank solution block uses {.callout-caution} instead of the collapsed tip style used by every other solution in the chapter. Renders as a visible orange warning block rather than a collapsed solution.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · lines 842, 879**

model4 is defined as a glmmTMB object (line 842) then reassigned to a gls object in the next solution block (line 879). Silent overwrite if students run sequentially. Same pattern with model6 and the corrs object.

**Proposed solution:**

------------------------------------------------------------------------

**Medium · lines 437, 578, 656**

rows = rownames(model.matrix(...)) used three times as an NA-removal workaround with no explanation. Non-idiomatic R; the standard approach is complete.cases() or na.omit(). Repeated unexplained pattern will confuse students.

**Proposed solution:**

------------------------------------------------------------------------

### Low / Style

**Low · line 514**

"We detrended space there could be spatial autocorrelation" — text appears to be missing a word or clause; the sentence is incomplete.

**Proposed solution:**

------------------------------------------------------------------------

**Low · line 1082**

cbind(survived, all - survived) — all is an R built-in function. The variable should be named n or total to avoid potential masking issues.

**Proposed solution:**

------------------------------------------------------------------------

*Total extracted: 91 issues*
