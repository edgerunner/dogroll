module Random.Words exposing (generator)

import Random exposing (Generator)


generator : Generator String
generator =
    Random.constant []
        |> Random.map2 (::) uniform
        |> Random.map2 (::) uniform
        |> Random.map2 (::) uniform
        |> Random.map (String.join "-")


uniform : Generator String
uniform =
    Random.uniform "the" list


list : List String
list =
    String.split "\n" words


words : String
words =
    """to
your
and
you
of
in
or
is
dice
it
if
with
that
for
character
conflict
have
roll
characters
at
as
raise
see
on
dogs
one
when
can
be
but
its
his
my
up
are
will
brother
fallout
from
them
an
out
get
this
so
two
demons
they
her
all
new
by
him
whats
say
how
take
some
do
player
dont
into
just
not
might
more
he
what
give
youre
only
has
stake
me
we
relationship
demonic
like
die
their
want
sin
make
person
trait
any
traits
blow
steward
go
faith
about
youve
life
something
choose
relationships
cant
way
follow
sum
sister
then
authority
put
town
stakes
someone
who
back
forward
name
branch
resolution
time
things
no
now
plus
acuity
influence
end
ceremony
thing
three
faithful
does
down
because
conflicts
coat
where
should
wants
wife
play
stats
our
first
doctrine
youll
over
hes
highest
people
each
old
other
best
than
too
keep
going
without
body
write
she
attacks
taking
game
gun
side
add
list
own
stop
stage
good
creating
here
been
murder
players
dog
temple
there
before
those
set
big
even
theres
sheet
doesnt
instead
use
would
lets
attention
next
isnt
again
right
everybody
hands
also
was
between
create
whether
call
many
shes
term
existing
raises
artax
service
already
pride
help
theyre
need
stewards
after
injustice
attack
hate
heart
block
dodge
size
cult
step
cadmus
initiation
maybe
whatever
else
always
office
through
assign
though
worship
bring
know
opponent
fighting
using
medical
malachi
ancients
long
four
left
itll
head
stay
most
look
mean
once
dead
corrupt
off
talking
since
hit
situation
love
shoot
away
theyll
family
earth
hold
come
sacred
around
which
place
friends
both
tree
bad
either
change
brothers
affected
subtract
injured
pcs
paragraph
serve
yourself
work
making
women
community
comes
very
enough
these
made
until
every
marry
done
leave
different
little
used
names
priesthood
sorcery
tell
second
single
try
remember
start
escalating
gets
continue
did
npcs
demon
attach
uncle
whole
teachers
were
got
nobody
badly
well
less
coats
had
hope
king
find
instance
however
youd
arent
calling
heres
means
fellow
sign
upon
able
physical
rules
order
having
stat
appropriate
ive
supernatural
took
equal
win
still
towns
benjamins
avigail
fight
taken
sometimes
takes
becomes
later
called
woman
together
expect
job
could
better
behind
never
couple
particular
against
ignore
hand
book
given
territorial
affair
secret
soon
makes
anyone
zachary
goes
same
figure
us
fact
punch
escalate
otherwise
arena
raising
seeing
weapon
talk
belonging
shooting
include
round
small
edie
usually
ask
wont
men
lots
higher
matter
above
let
such
send
needs
course
another
doing
convert
being
groups
sinner
heresy
somewhere
whose
lose
care
kill
act
deserves
ahead
during
initiatory
excellent
belongings
crap
son
launch
whos
circumstances
works
depends
listed
action
rolled
id
shot
short
giving
scene
description
bump
live
thinks
sense
opponents
to
yours
axe
wilhelmina
tom
jonas
few
notice
home
house
case
alone
normal
much
somebody
companions
eventually
released
turn
deal
husbands
happens
working
join
mountain
mind
someones
available
ill
perform
invoking
simply
reciting
incorporate
possible
least
baby
you
relevant
become
form
fine
guy
think
joseph
itself
consider
really
probably
number
usual
continuum
sorcerers
sort
recap
shopkeeper
turns
throw
pull
playing
opening
zeke
murders
example
experience
store
contributes
possessed
friend
vs
sees
came
middle
games
bam
skip
aunt
whiskey
cyrus
meg
process
spend
months
bridal
falls
city
initiates
rolling
years
duty
hurt
ride
survive
scripture
moment
point
responsibility
sisters
poor
participate
told
laying
field
ones
beat
under
getting
hard
anybody
fall
spiritual
level
non
far
husband
release
wait
everyone
eyes
kind
holding
themselves
complicated
strong
sins
chapter
ceremonies
anointing
carry
elses
faiths
singing
whichever
elements
marriage
sick
command
ghost
proud
free
power
habitual
justify
immediately
lead
yet
niece
worst
josephs
clear
conscience
guilt
story
happen
grab
outcome
directly
creation
suggest
draw
guns
gone
establish
pretty
relation
push
including
aside
ends
stuff
everything
jerk
improvised
thought
previous
harm
current
death
special
corner
thug
institution
sorcerer
break
hay
rake
contribute
ceremonial
dying
standards
hear
communitys
eleazer
alise
especially
benjamin
burned
grandmother
cousin
bethia
trek
initiated
training
fewer
necessarily
questions
theology
initiate
prophets
receive
says
soul
personal
naturally
move
teaching
while
parents
consecrated
blessing
arm
himself
high
families
often
range
early
playtest
onto
reasons
route
times
holder
run
decided
unless
reason
east
born
mountains
raised
converted
wicked
culture
reflect
addition
generally
forehead
praise
hymns
whenever
third
mark
babys
depart
commands
persons
minute
duties
bringing
world
huh
wrong
creates
leads
resentful
crops
victim
arrive
guys
sinning
daughter
resolve
past
support
worse
horse
check
cares
street
rage
position
actions
author
opportunity
moments
dirt
front
killed
said
among
seems
assigned
accomplished
table
within
night
sure
bullets
slide
begin
shut
feel
mine
atall
gming
participating
blows
words
punches
decide
farm
tool
opens
blood
part
per
seen
reversing
attacker
bullet
advantage
procedure
this
cases
forth
last
wouldnt
way
wind
shopkeepers
catch
potential
worth
hitting
hammer
treat
timing
brings
cut
losses
suffers
launches
healer
following
choices
explain
thugs
beginning
multiple
cool
jumps
clearly
busy
wrong
lot
named
do
apply
learn
stilling
murdered
room
crime
box
landscape
pang
inspiring
organized
congregation
grabby
happy
census
protect
elsa
judith
calls
starting
ending
winter
goal
goals
proven
train
preach
teach
apart
sanctify
light
primary
grandmothers
expected
skills
necessary
party
puts
stitch
bless
beautiful
communities
maintain
preserve
original
allowed
across
patch
loved
stabbed
typical
murdering
stitched
wool
wear
thrown
whoever
rarely
fend
rough
intuition
unassigned
inspired
return
periodically
twice
annually
faithfully
former
although
today
future
demand
finished
actually
theologian
visit
ceremonially
confirmation
supposed
waiting
knows
according
dies
oldest
built
faces
subtler
powerful
humble
compassion
twin
odds
comfort
understand
innocent
remembering
describe
grew
grove
river
committing
included
anoint
marking
full
declaring
top
palm
fingers
rituals
holders
sent
acceptable
branches
specific
solemnize
passages
wed
heal
lay
healing
declare
corpse
beliefs
count
babies
children
branchs
challenges
harvest
perfect
mostly
enacted
bold
victims
corporeal
failing
serves
threaten
blame
incorrect
observances
subtle
outright
followers
nobodys
terrible
year
winning
killing
shouldnt
coming
keeping
steps
problem
destroying
show
burst
him
my
judge
difference
burn
lives
leaving
thats
gonna
easy
occasionally
leading
divvy
trouble
rest
pool
emphasize
experienced
nothing
the
ridden
straight
and
initiation
noticed
clowns
related
building
open
struggle
restore
mouth
along
doubts
history
effective
interesting
money
father
bitter
humiliated
sons
determine
provide
desperate
bones
figuring
match
putting
depending
argument
whoevers
deals
simple
shed
noon
recall
relations
oughta
neither
ground
describing
response
discard
counts
saw
rather
me
reversed
reversal
yes
surprised
gunfight
believe
force
barrel
merely
gunshot
looking
butt
dumb
dangerous
progress
underway
incorporating
willing
reserved
mix
reroll
represents
terms
consequences
bear
rated
lasting
erase
rewrite
permanent
damage
reduce
recover
healers
tools
inflict
inflicts
general
exactly
extra
pass
aid
status
via
situations
effects
newly
rhythm
starts
boldface
marks
may
stand
hook
you
obvious
group
objects
spent
entire
element
valid
uses
useful
something
manifestation
didnt
whats
no
dig
important
natural
convince
saying
trust
givable
hedged
methods
sessions
etc
gives
rolls
instructor
outside
flips
nickel
grandfathers
fear
riding
nights
tracked
bowers
red
mud
creek
bed
handle
voices
control
trees
inside
forcing
anything
board
present
overt
injustices
sinners
arrival
specifics
neighbors
exceptionally
righteous
specifically
fertility
barren
expectations
causes
conclude
doctrines
allows
true
lover
religious
heretics
priest
members
possession
just
setup
repeat
leader
kills
sentence
living
negligible
civic
farmhands
fire
skipped
theyd
pronounce
okay
rob
buy
deputy
sheriff
conceive
performing
wedding
wilhelmina
sorcerously
stillborn
wasnt
malachis
sex
formally
interviews
goodbyes
pack
interview
assignment
schedule
continuous
therell
dozen
fifteen
prove
cull
exhaust
humiliate
stress
disappoint
tempt
scare
provoke
overwhelm
persevere
patient
discerning
capable
confident
educate
cosmology
demonology
solid
grounding
studies
invest
consecrate
oaths
worthy
inspire
predict
background
effect
scriptural
scholar
fellows
read
meanwhile
learning
proving
honor
mothers
aunts
reflects
foremost
boys
oversee
project
coordinate
efforts
traditional
extended
state
unfinish
toward
package
containing
letters
wishing
actively
fierce
beating
patching
piecing
repairing
replacing
earlier
packed
carefully
torn
battered
ruined
offices
replace
vestments
accompany
initiating
powerfully
significant
picture
faded
stretched
shoulders
shirt
stain
crude
boyfriend
ago
esteem
regards
mixed
feelings
sall
home
screwing
piecework
imagine
remnant
reef
canvas
wholly
similar
cracks
silhouette
plain
intuitive
component
watched
known
based
reassign
local
man
considered
preferentially
technically
teacher
served
prestigious
suitors
suitor
seventy
court
propose
respect
administration
regarded
advisor
inclinations
treats
complaints
seriously
complete
urgent
proxy
message
longer
dodgy
honorable
met
unfaithfully
punished
jobs
impossible
expects
guaranteed
kitchens
stables
sweeping
household
chores
desert
thirties
suppose
converts
practically
westward
ten
adults
adoptive
leaner
hair
quite
fit
subject
prejudice
outrightly
hostile
superstitious
lazy
dirty
uniquely
noble
admiring
antiquity
heritage
standard
expecting
insightful
fail
adopted
likely
recently
prejudices
upbringing
meaning
peoples
ancestors
looked
spirits
led
provided
native
religion
thoughtless
habits
childhood
evil
minds
balance
serving
feels
strongly
interested
learned
institutions
places
habitually
struggling
particularly
resolute
speaking
meanwhile
idea
investing
clay
jar
contact
skin
symbol
stylized
shoulder
wide
spread
sung
ritual
suit
common
by
ancient
moved
prophecy
foreheads
recite
health
sing
drive
bolster
hospitable
dedicate
charge
corpses
folk
died
passes
sanctifies
presents
temptation
remain
judgment
official
travels
routinely
mail
news
officiate
holy
naming
dedicating
solemnizing
weddings
sanctifying
honored
participation
deliver
interpretation
needed
consult
social
functions
celebrations
digging
blizzard
immediate
acute
shake
kiss
looks
breaks
protected
intermediary
raids
outlaws
soldiers
disease
disaster
drought
storms
misfortune
inappropriate
changes
blatant
demonism
begins
attacking
ever
obedience
premeditated
ultimate
causing
sinned
peg
cousins
blight
hidden
gradually
somehow
won
several
knowing
blessings
honestly
harder
apple
acting
justly
complain
acts
behalf
unteachable
unreformable
handedly
guidance
repentance
cultivate
tells
invested
worry
septic
wound
thousand
resentments
tear
pieces
stone
caught
bloodshed
damnation
drag
all
investment
zacharys
salvation
your
wound
save
body
remorseless
monster
angel
progresses
problematize
arrogance
bloodlust
remorse
contrition
inspiration
redemption
grace
matters
stories
it
retire
farmer
quit
pointless
arbitrary
chosen
wrap
exiting
epilogue
eulogy
highlights
rush
hurry
collectively
recognition
satisfied
blank
definitely
available
slot
shuffle
emphasized
shift
equip
forget
introduced
piece
sitting
contributing
represented
minimize
bounds
why
wgming
hile
developing
whore
competent
redirect
asking
pointing
responsibilities
entails
impulse
hardness
underneath
cements
horses
accomplish
admiration
hostlers
arranging
mounts
effectiveness
barely
luck
pressures
reassure
remind
commitments
exorcist
gunslingers
battling
ghosts
striking
sparks
follow
lead
stuck
ive
exorcised
demon
speaks
dreams
healer
lines
respond
streak
conclusions
file
nows
freud
proved
trained
educated
suited
genre
rounded
supernaturally
underlying
issues
procurer
nephew
fourteen
stealing
thievery
grieving
lost
innocence
overview
wins
throughout
reversals
escalation
daring
advances
retreats
broken
cutting
betrayals
juicy
goodness
interest
represent
bargaining
reactions
weight
consequence
shrugs
establishing
setting
throws
plays
betting
poker
raises
see
bowl
effectively
representing
knives
ribs
losing
thatll
fate
suggestions
toss
mention
features
environment
meets
rut
road
nearest
hundred
feet
swaying
gold
wheat
sky
insane
summer
blue
smoothbore
carbine
jaw
clenched
currently
limited
specify
default
excluding
attack
violent
helped
responds
exceed
standing
defends
lands
reacts
nature
finally
gain
staying
hey
people
lets
get
boy
in
trade
dont
conscience
you
son
stands
sails
maybe
forget
whatevers
fist
outstanding
leftover
grim
jaw
bruised
grumbling
started
punching
adjust
eyeglasses
trigger
hadnt
handy
disarming
enemies
shove
upward
air
hands
weve
escalated
treatment
anyhow
paying
gunless
inflicted
received
others
anyway
intended
purpose
spyglass
unintended
pistols
wedging
doorjamb
passing
below
eye
participant
mechanically
written
realize
remaining
ceding
sizes
hurtin
avoid
events
justified
asked
purported
resolved
festering
agrees
follow
follows
ended
considerations
participants
tries
admitting
fails
rerolling
option
calculate
hire
fends
manages
nearby
stable
shots
fired
ugly
track
captured
reveals
attempted
arson
due
heat
frankly
active
regard
committed
resisted
commission
applicable
rounds
helps
outline
stripped
changed
fleshed
theyve
barn
zeke
twists
grip
jacket
ankle
windmill
arms
wall
pulls
swings
freely
chest
knock
outa
myself
whish
hah
hey
lock
urk
explicit
none
reverse
obviously
graciously
withdraw
reasonable
helpful
insist
moreover
borrowed
helping
crashing
reserve
zeke
shouts
shoves
souls
performed
sorts
pace
determines
receives
includes
strictly
applies
low
edge
roll
influence
generalized
badness
discovered
critically
scoop
hacked
hanged
clubbed
strangled
stomped
fallen
drowned
opposition
ithink
second
session
advice
mechanics
section
instead
abdicate
truth
sinnin
ways
forever
undo
done
engage
wrestle
pause
breathe
easier
now
creative
tension
dull
pushing
smaller
tend
trick
roughly
large
real
question
winds
blowing
grandiose
repent
repent
spilling
blood
outcomes
observation
legit
observe
dissatisfied
frowns
withdrawing
language
weak
reaches
everyones
meet
critical
pay
press
resolving
fourth
fifth
sixth
seventh
eighth
escalates
incorporated
interpreting
rerolled
accept
arguments
chases
fistfights
shootouts
preaching
crowd
exorcising
flexible
thinking
split
seconds
outshoot
cans
scarecrows
trigger
suns
glare
distance
nickels
flickering
insistence
kid
breath
target
taught
miss
presumptuous
foolish
draws
dusty
clock
click
noise
strikes
instant
gong
flexing
narrowing
bird
flying
sun
thoughts
flinches
hesitations
rightness
tricks
hills
brooks
scrubland
snapshots
montage
sequence
movie
challenging
on
day
riding
gave
swearing
council
task
two
stable
flashbacks
hideout
inclined
flashback
killings
bent
doorframe
bed
bodyguards
mood
creep
ridge
were
careful
sight
bam
billy
glint
dives
splatter
gore
billys
ambush
creeps
waking
asleep
sleep
gashes
bad
awake
motion
knife
hand
startled
roleplaying
enemy
sneaks
hits
axe
cheating
hosed
warning
gmed
bunch
rpgs
rushing
falling
unconscious
hearing
angels
spurting
pulse
deceased
welcoming
exhortation
fat
ouch
companion
rushes
flashy
colorful
creepy
bending
cold
lies
answer
sweet
chills
disembodied
reenactment
pain
accomplishment
prepared
huge
marble
carved
prayers
crowbar
possess
succeeds
pantomime
ruse
escape
battering
whispering
ears
great
magically
charged
western
version
chinese
rode
spooked
bucked
jump
swinging
repeater
fanning
sweep
spark
mighty
righteousness
drop
gun
fun
structure
vineyard
anothers
details
etcetera
contributions
role
adversity
oppose
initiations
presence
various
material
forms
assess
isolate
endanger
survival
exacerbate
prosper
oppress
threat
interests
holdings
except
sexual
lust
souring
marital
blighting
herds
fecund
distrust
spouses
constrain
develop
defy
caution
false
adopting
numb
alternately
stepping
correct
fault
further
flaw
allowing
concrete
examples
parley
quarters
moon
sabbath
convenience
god
human
wisdom
keys
heaven
outward
expression
god
practices
individual
commits
adopts
weird
heretic
develops
anywhere
decent
misled
sub
attendants
covert
ruin
agenda
adopt
cults
vulnerable
serious
passion
deadly
assault
wholly
tidy
sin
triangle
stolen
cattle
cause
entirely
tenor
senseless
ritualistic
perhaps
occult
significance
motives
upstanding
tip
sinister
promises
covered
earnest
claim
whove
seems
enough
enough
busy
secular
fashion
thereby
introduce
legitimate
involved
cross
purposes
unrelated
budding
prides
wanted
session
corresponding
wrongness
backward
spectacularly
scroll
problems
unaddressed
advantaged
disadvantaged
invents
witness
repair
failure
tenet
expresses
practice
worshippers
threats
unresolved
ladder
boxelder
canyon
info
reporting
warrant
familys
maintenance
spends
pestering
grief
sells
sly
church
meeting
pissed
seem
baked
blames
bigotry
invalid
praying
younger
farmhand
arrived
listening
raving
business
drinking
paid
wage
malice
undermine
close
demonsll
overtime
implicate
twist
sooner
overthrow
whisper
alive
whitechurch
resisting
courtship
consequently
obsessed
buying
gifts
afford
burdening
prices
profit
young
rich
lording
breaking
faster
aged
uncles
values
independence
air
content
talked
noses
spiritualist
atheist
dogmatist
wing
assuage
loves
harvest
congregations
property
applying
scarcity
pressure
restock
tower
righteously
monopolizing
manipulating
time
sex
public
regard
correctly
infidelity
concludes
incorrectly
talks
established
herself
prophet
trying
edies
robbing
theirs
child
womb
yesterday
unborn
healthy
prevail
skeptical
closer
shell
mislead
mother
he
meant
dead
stillbirth
sorceress
earliest
played
respectively
willingly
becoming
trophy
boy
access
quo
inevitably
spawn
approval
breakaway
liked
blessed
charming
suspicious
brother
wife
hide
delighted
aback
swayed
sympathetic
hopes
confront
principles
spill
avagails
satisfying
shared
aha
crossing
separate
scenes
whom
bundle
convenient
lie
greater
chance
risk
shunning
public
funny
his
wrote
yeah
implied
cast
brother
abusing
progression
judged
viscerally
objective
foregone
lied
fascinating
unbelievably
why
"""
