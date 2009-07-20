{application, baberl,
  [{description, "iconv driver"},
   {vsn, "0.0.1"},
   {modules, [baberl]},
   {mod, {baberl_app, []}},
   {registered, [baberl_sup, baberl]},
   {applications, [kernel, stdlib]}]}.
