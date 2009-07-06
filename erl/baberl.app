{application, baberl,
  [{description, "iconv driver"},
   {vsn, "0.0.1"},
   {modules, [baberl_driver, baberl_app, baberl_sup, baberl]},
   {registered, [baberl_sup, baberl]},
   {applications, [kernel, stdlib]},
   {mod, {baberl_app, []}}]}.
