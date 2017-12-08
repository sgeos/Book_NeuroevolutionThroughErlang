%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
% Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
% All rights reserved.
%
% This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE
% file that accompanies this project for the terms of use.

-module(template).
%-compile(export_all).
-export(
  [
    f/0,
    f/1
  ]
).
-include("template.hrl").

% ...
f() ->
  f(ok).

f(Acc) ->
  Acc.

