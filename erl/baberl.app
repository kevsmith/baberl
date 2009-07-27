{application, baberl,
  [{description, "iconv driver"},
   {vsn, "0.0.2"},
   {modules, [baberl, baberl_app, baberl_loader, baberl_sup, baberl_wordnet]},
   {mod, {baberl_app, []}},
   {registered, [baberl_sup, baberl, baberl_wordnet]},
   {applications, [kernel, stdlib]}]}.
