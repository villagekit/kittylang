# Diátaxis: language patterns and boundary tests

Condensed from https://diataxis.fr/. Use alongside [SKILL.md](SKILL.md).

## Language patterns

**Tutorial** - first-person plural; the tutor and learner work together.
- "In this tutorial, we will …" (never "you will learn …")
- "First, do x. Now, do y. Now that you have done y, do z."
- "The output should look something like …"
- "Notice that … Remember that … Let's check …"
- "If the output doesn't show …, you have probably forgotten to …"
- "We must do x before y because … (see [explanation] for details)."
- Close by naming the achievement: "You have built a working …"

**How-to guide** - conditional imperatives addressed to a working user.
- "This guide shows you how to …"
- "If you want x, do y. To achieve w, do z."
- "If this, then that. In the case of …, an alternative approach is to …"
- "Refer to the x reference for a full list of options."
- Titles: good "How to integrate performance monitoring"; bad "Integrating
  performance monitoring"; very bad "Performance monitoring".

**Reference** - declarative present tense about the machine.
- "X is … X returns … The default is … Defined in … Available as …"
- Lists and tables of commands, options, flags, limits, error messages.
- Warnings: "You must use a. You must not apply b unless c. Never d."
- Absent: instructions, recommendations, rationale.

**Explanation** - discursive, connective, willing to judge.
- "The reason for x is that historically, y …"
- "W is better than z, because …"
- "An x in system y is analogous to a w in system z. However …"
- "Some users prefer w (because z). This can be a good approach, but …"

## Boundary tests

**Tutorial vs. how-to guide** (both guide action; the most common conflation).
The test is study vs. work. A tutorial is a lesson: the author is responsible
for the learner's success, the path is single and contrived, safety is
guaranteed, and what the learner builds matters less than what they gain. A
how-to guide serves a user at work: they are competent, they own the risk, and
the path branches with the real world. This is not basic vs. advanced: a
tutorial can teach something advanced, and a how-to guide can cover something
basic. Ask which need it serves, not how hard it is.

**Reference vs. explanation** (both carry facts; easy to slip between).
The test is: would someone consult this while working, or read it after
stepping away to think? Rules of thumb from the site:
- If it is boring and unmemorable, it is probably reference.
- Lists of things (classes, methods, options) and tables of information
  generally belong in reference.
- If you could imagine reading it in the bath, it is explanation.
- If it answers "Can you tell me about …?", it is explanation.

**Reference vs. how-to guide** (both serve work).
Reference is led by the product and describes; a how-to guide is led by a user
need and directs. "The `--force` flag skips validation" is reference. "To
recover a corrupted index, run … with `--force`" is how-to.
