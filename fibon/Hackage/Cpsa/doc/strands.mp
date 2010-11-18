% Strand macros

%% John D. Ramsdell--May 2008,
%% The MITRE Corporation, Bedford, MA.

% For simple drawings, use the strand macro.

% Use these macros when drawing strand edges.  The cmstrands package
% redefines these two macros.

% Draw a message edge
def drawmsg(expr p) =
  drawarrow p;
enddef;

% Draw a strand succession edge
def drawsucc(expr p) =
  drawarrow p;
enddef;

% Strand drawing macros

% Box the nodes of a strand.  The origin of the strand is the center
% of the last strand node.  The name is a picture for the first node,
% and node is a picture for the successive nodes.
vardef boxstrand@#(expr height, origin, name, node) =
  boxit.@#1(name);
  for i = 2 upto height:
    boxit.@#[i](node);
    xpart @#[i-1].s = xpart @#[i].n;
  endfor;
  @#[height].c = origin;
enddef;

% Space a pair of strand nodes using the given height and separation
% delta.
vardef spacestrand@#(expr pos, delta) =
  ypart @#[pos].s = delta + ypart @#[pos+1].n;
enddef;

% Layout strand by making the distance been successive nodes the given
% separation delta.
vardef layoutstrand@#(expr height, delta) =
  for i = 1 upto height - 1:
    spacestrand@#(i, delta);
  endfor;
enddef;

% Link the nodes of a strand.
vardef linkstrand@#(expr height) =
  for i = 2 upto height:
    drawsucc(@#[i-1].s -- @#[i].n);
  endfor;
enddef;

% Draw a strand with equally spaced nodes.  The origin of the strand
% is the center of the last strand node.  The the distance between
% successive nodes is delta.  The name is a picture for the first
% node, and node is a picture for the successive nodes.

% A strand is drawn bottom up because the size of the label in the
% first box may vary.

vardef strand@#(expr height, origin, delta, name, node) =
  boxstrand@#(height, origin, name, node);
  layoutstrand@#(height, delta);
  for i = height downto 1:
    draw pic @#[i];
  endfor;
  linkstrand@#(height);
enddef;

% Directed term macros

% Draw an inbound directed term at a given strand position.  The width
% is the length of the first arrow.
vardef inbnd@#(expr pos, width, term) =
  pair d;
  d = (width + xpart @#[1].e, ypart @#[pos].e);
  drawmsg(d .. @#[pos].e);
  label.rt(term, d);
enddef;

% Draw an outbound directed term at a given strand position.  The
% width is the length of the first arrow.
vardef outbnd@#(expr pos, width, term) =
  pair d;
  d = (width + xpart @#[1].e, ypart @#[pos].e);
  drawmsg(@#[pos].e .. d);
  label.rt(term, d);
enddef;
