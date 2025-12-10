% presses_joltage_and_target.mzn
% Enforce:
%   (1) exact per-counter joltage totals
%   (2) final switch pattern target (parity)
% while minimizing total presses.

int: W;                % number of counters
int: M;                % number of buttons

array[1..W,1..M] of int: effects;   % effects[i,j] = 1 if button j increments counter i
array[1..W] of int: joltage;        % required exact change counts
array[1..W] of int: target;         % final parity (0/1) pattern

% upper bound for presses per button
int: globalMaxPress = 300;

% decision variables:
array[1..M] of var 0..globalMaxPress: presses;
array[1..M] of var 0..1: pressed;    % pressed[j] = presses[j] mod 2

% total number of presses
var 0..(M * globalMaxPress): total_presses = sum(j in 1..M)(presses[j]);

% link parity: odd/even view of presses
constraint forall(j in 1..M)(
    pressed[j] = (presses[j] mod 2)
);

% compute per-counter totals
array[1..W] of var 0..globalMaxPress: contributions =
  [ sum(j in 1..M)(effects[i,j] * presses[j]) | i in 1..W ];

% (1) exact joltage totals
constraint forall(i in 1..W)(
    contributions[i] = joltage[i]
);

% (2) parity constraint to match the target switch pattern
constraint forall(j in 1..M)(
  (presses[j] mod 2 = 0 /\ pressed[j] = 0) \/ (presses[j] mod 2 = 1 /\ pressed[j] = 1)
);

% minimize total button presses
solve :: int_search(presses, first_fail, indomain_min) minimize total_presses;

output [
  "presses: ", show(presses), "\n",
  "pressed (odd/even): ", show(pressed), "\n",
  "contributions: ", show(contributions), "\n",
  "joltage: ", show(joltage), "\n",
  "target: ", show(target), "\n",
  "total_presses: ", show(total_presses), "\n"
];
