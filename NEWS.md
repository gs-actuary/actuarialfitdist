# actuarialfitdist 0.1.0

* Numerical warnings from invalid optimizer trial points are now handled internally; such points receive a penalized likelihood while invalid final fits still fail explicitly.

* Initial release.
* Fits common actuarial severity distributions to ground-up losses with deductible truncation and limit censoring.
* Supports optional linear severity scaling, likelihood weights, fixed parameters, candidate comparison, and spliced body-tail models.
* Generates limited expected severities, LERs, ILFs, deductible factors, combined rating factors, and valuation-adjusted expected payments.
* Includes simulation-based goodness-of-fit diagnostics, scaling diagnostics, layer diagnostics, Hessian inference, and basic bootstrap support.
