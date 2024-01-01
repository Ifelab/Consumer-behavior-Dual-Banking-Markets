;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Leveraging Ethics to Expand Islamic Banks’ Consumer Base:;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; A fuzzy Agent Based Simulation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VERSION 1.0 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Contact information:
;; Wail Aaminou
;; IFE LAB, Ecole Mohammadia d'Ingénieurs, Mohammed V University, Rabat, Morocco.
;; e-mail: mohamed.wail@gmail.com


extensions [fuzzy] ;; This simulation relies on the fuzzy-logic-extension-model version 1.1 developed by Doina Olaru & Luis R. Izquierdo. The fuzzy-logic-extension file “Fuzzy.zip” has to be unzipped and placed in the same directory as the Netlogo file

;;;;;;;;;;;;;;;;;
;;; Variables ;;;
;;;;;;;;;;;;;;;;;


Globals [rhetoric_duration found s] ;; Global variables : "rhetoric_duration" and "found" are used in the computing_rhetoric_duration procedure. "s" is used in the fuzzy set graphs

breed [consumers consumer] ;; consumers' agents

breed [banks bank] ;; Banks' agents

consumers-own [client score_ethicbank score_rhetoricbank asymmetry_coef proba_ethicbank proba_rhetoricbank proba_conventionalbank unhappy CSR_satisfactory CSR_unsatisfactory likely_choice very_likely_choice unlikely_choice very_unlikely_choice]
;;(client : new -> newly created, selected -> new potential client, 0 -> Conventional, 2 -> rhetoric bank, 1 -> ethical bank) | score_bank : ethical score as viewed by the client | unhappy ( 0 -> No, 1 -> Yes)

banks-own [nature] ;; (rhetoric or ethic)


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;


to setup

  clear-all

  reset-ticks

  set found 0

  create-banks 1 ;; creates one ethical bank

  [
    Set Nature "rhetoric"
    setxy distance_banks * 2 distance_banks * 2 ;; places the ethical bank in the grid
    set shape "star"
    set color black
  ]

  create-banks 1 ;; creating one rhetoric bank

  [
    Set Nature "ethic"
    setxy ( - distance_banks * 2) (- distance_banks * 2) ;; places the ethical bank in the grid
    set shape "star"
    set color white
  ]

  seed_ethical_consumers ;; creates consumers with ethical orientation

  if Speed_of_learning = "No learning"

  [set rhetoric_duration "Non applicable"] ;; if learning is not enabled, the rhetoric bank will always have clients


end

to seed_ethical_consumers ;; creating consumers with ethical orientation

   ask patches with [not any? consumers-here] ;; every consumer in a separate patch (cell)
   [ sprout-consumers 1 [set-as-ethic] ]

end


to set-as-ethic ;; creating consumers with ethical orientation

  set client "new" ;; tagged : newly created. Already clients of the conventional bank
  set unhappy "0"
  set shape "square"
  set color green
  set asymmetry_coef 1 ;; Strating with full asymmetry for all consumers
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run-time procedure ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


to go

  ifelse Speed_of_learning = "No learning" ;; when learning is disabled, skip the learning and peer influence procedures

   [

      while [ticks < 130]
        [

      select_new_potential_consumers

      choose_bank

      tick
        ]

   ]


[
  while [found < 20]  ;; When Learning is enabled, run these instructions as long the rhetoric bank still has customers. For more details, refer to computing_rhetoric_duration procedure.

  [
    select_new_potential_consumers

    choose_bank

    consumers_learning

    peers-influence

    computing_rhetoric_duration

    tick

]
]


end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Supporting Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to select_new_potential_consumers
;; SELECTING CONSUMERS THAT WILL CHOOSE BETWEEN THE ETHICAL BANK, THE RHETORIC BANK AND THE CONVENTIONAL BANK

 if count (consumers with [client = "new"]) > 0

 [


  ifelse ticks = 0 ;; first period

 [

 ask n-of round ( F (1) ) consumers with [client = "new"]

 [ set client "selected"

   ]
 ]

 [

   let beta round ( F (ticks + 2) - F (ticks + 1) )

   ifelse beta < count (consumers with [client = "new"])  ;; this is to avoid rounding problems
   [

 ask n-of beta consumers with [client = "new"] ;; selection is conducted among clients that were not selected in previous periods

 [ set client "selected"

   ]
 ]

   [
   ask consumers with [client = "new"] ;; selection is conducted among the final remaining clients

 [ set client "selected"

   ]
   ]

 ]


ask consumers with [client = "selected"]

[
set score_ethicbank 10 - asymmetry_coef * ((((distance bank 1) / ( 4 * sqrt (2) )) ^ 2)) / 10  ;; normalizing the score to 10 (the max value). The largest distance in the grid is 40*sqrt (2)

set score_rhetoricbank asymmetry_coef * (( (distance bank 0) / ( 4 * sqrt (2) ) ) ^ 2) / 10  ;; normalizing the score to 10 (the max value). The largest distance in the grid is 40*sqrt (2)

create_fuzzy_sets ;; creating fuzzy sets for the selected agents

]

 ]

end


to-report F [period]
;; TOTAL NUMBER OF POTENTIAL CONSUMERS INFORMED ABOUT BANK POSITIONED ON ETHICS. INFORMATION DRIVERS ARE P : MASS MEDIA EFFECT AND Q : WORD OF MOUTH

  report  count (consumers) * ( 1 - P * exp ( - ( P + Q) * period )) / ( 1 + Q * exp ( - ( P + Q) * period ) / P)

end



to create_fuzzy_sets
;; CREATES FUZZY SETS FOR SELECTED CUSTOMERS

  set CSR_satisfactory fuzzy:gaussian-set (list 10 (2 + var noise) [0 10]) ;; A Membership score of 1 gives full satisfaction. the noise factor captures the population ethical diversity
  set CSR_unsatisfactory fuzzy:gaussian-set (list 0 (2 + var noise ) [0 10])   ;; A Membership score of 1 gives zero satisfaction, the noise factor captures the population ethical diversity
  set likely_choice fuzzy:gaussian-set [10 1 [0 10]]
  set unlikely_choice fuzzy:gaussian-set [0 1 [0 10]]
  set very_likely_choice fuzzy:power likely_choice 2 ;; Using the function "power" to represent the linguistic hedges very
  set very_unlikely_choice fuzzy:power unlikely_choice 2 ;; Using the function "power" to represent the linguistic hedges very

end

to-report score-ethical [ethicbank rhetoricbank] ;; enters scores as inputs
 ;; COMPUTES RESHAPED CONSEQUENTS FOR EACH RULE

  let R1 fuzzy:and-rule (list (list ethicbank CSR_satisfactory) (list rhetoricbank CSR_unsatisfactory)) very_likely_choice
  let R2 fuzzy:and-rule (list (list ethicbank CSR_unsatisfactory) (list rhetoricbank CSR_satisfactory)) very_unlikely_choice
  let R3 fuzzy:and-rule (list (list ethicbank CSR_satisfactory) (list rhetoricbank CSR_satisfactory)) unlikely_choice
  let R4 fuzzy:and-rule (list (list ethicbank CSR_unsatisfactory) (list rhetoricbank CSR_unsatisfactory)) very_unlikely_choice

  let score-fuzzy-set (runresult (word "fuzzy:" type-of-aggregation  "(list R1 R2 R3 R4)"))  ;;(runresult (word "fuzzy:" type-of-aggregation " list-of-rules"))
  let score (runresult (word "fuzzy:" type-of-defuzzification "-of score-fuzzy-set"))
  report score

end

to-report score-rhetoric [ethicbank rhetoricbank]
 ;; COMPUTES RESHAPED CONSEQUENTS FOR EACH RULE

  let R1 fuzzy:and-rule (list (list ethicbank CSR_satisfactory) (list rhetoricbank CSR_unsatisfactory)) very_unlikely_choice
  let R2 fuzzy:and-rule (list (list ethicbank CSR_unsatisfactory) (list rhetoricbank CSR_satisfactory)) very_likely_choice
  let R3 fuzzy:and-rule (list (list ethicbank CSR_satisfactory) (list rhetoricbank CSR_satisfactory)) likely_choice ;; the rhetoric bank has a cost advantage compared to the ethical bank
  let R4 fuzzy:and-rule (list (list ethicbank CSR_unsatisfactory) (list rhetoricbank CSR_unsatisfactory)) very_unlikely_choice

  let score-fuzzy-set fuzzy:sum (list R1 R2 R3 R4)
  let score fuzzy:COG-of score-fuzzy-set
  report score

end

to-report score-conventional [ethicbank rhetoricbank]
;; COMPUTES RESHAPED CONSEQUENTS FOR EACH RULE

  let R1 fuzzy:and-rule (list (list ethicbank CSR_satisfactory) (list rhetoricbank CSR_unsatisfactory)) very_unlikely_choice
  let R2 fuzzy:and-rule (list (list ethicbank CSR_unsatisfactory) (list rhetoricbank CSR_satisfactory)) very_unlikely_choice
  let R3 fuzzy:and-rule (list (list ethicbank CSR_satisfactory) (list rhetoricbank CSR_satisfactory)) very_unlikely_choice
  let R4 fuzzy:and-rule (list (list ethicbank CSR_unsatisfactory) (list rhetoricbank CSR_unsatisfactory)) very_likely_choice


  let score-fuzzy-set fuzzy:sum (list R1 R2 R3 R4)
  let score fuzzy:COG-of score-fuzzy-set
  report score

end

to-report var [noise_input]
;; REPORTS A RANDOM NOISE NUMBER IN THE INTERVAL [-NOISE/2 NOISE/2]

  report (random-float (noise_input) - noise_input / 2)

end



to choose_bank
;; COMPUTES THE PROBABILITY OF CHOOSING A BANK

ask consumers with [client = "selected"]

[

  set proba_ethicbank round ( 100 * ( score-ethical score_ethicbank score_rhetoricbank) / ( (score-ethical score_ethicbank score_rhetoricbank) + (score-rhetoric score_ethicbank score_rhetoricbank) + (score-conventional score_ethicbank score_rhetoricbank)))
  set proba_rhetoricbank round (100 * ( score-rhetoric score_ethicbank score_rhetoricbank) / ( (score-ethical score_ethicbank score_rhetoricbank) + (score-rhetoric score_ethicbank score_rhetoricbank) + (score-conventional score_ethicbank score_rhetoricbank)))
  Set proba_conventionalbank round (100 * ( score-conventional score_ethicbank score_rhetoricbank) / ( (score-ethical score_ethicbank score_rhetoricbank) + (score-rhetoric score_ethicbank score_rhetoricbank) + (score-conventional score_ethicbank score_rhetoricbank)))


let alpha random-float (100)

ifelse  proba_ethicbank >= alpha

 [ set client "1" set color white] ;; ethical bank client

 [ ifelse (proba_rhetoricbank + proba_ethicbank >= alpha )


   [set client "2" set color black] ;; rhetoric bank client

   [set client "3" set color green] ;; stays within the conventional bank

 ]

 ]

end

to peers-influence
;; IMPLEMENTS PEER INFLUENCE (CONTAGION) THROUGH THE LINEAR THRESHOLD MODEL

    ask consumers with [client = "2" and unhappy = "0" ] ;; rhetoric bank's clients who are still happy
    [
      let angry_peers count (consumers-on neighbors) with [unhappy = "1"]

      if angry_peers >= threshold  [set unhappy "1" set color yellow] ;; if unhappy peers exceed the threshold, the client of the rhetoric bank becomes unhappy

     ]

End

To consumers_learning
;; LEARNING CAPABILITIES FOR RHETORIC BANK's CUSTOMERS

  ask consumers with [client = "2" and unhappy = "0" ] ;; rhetoric bank clients who are still happy

    [
    if score_rhetoricbank != 0


    [ set asymmetry_coef asymmetry_coef - 1 / ((speed speed_of_learning) * score_rhetoricbank)

    if asymmetry_coef <= 0 [ set unhappy "1" set color pink]
    ]

    ]

end


to-report speed [learning_mode]
 ;; REPORTS A LEARNING COEFFICIENT DEPEDING ON THE SPEED OF LEARNING

  if learning_mode = "Regular" [report 10]
  if learning_mode = "Fast" [report 5]
  if learning_mode = "Slow" [report 20]

end


To computing_rhetoric_duration
  ;; COMPUTES RHETORIC DURATION : THE NUMBER OF PERIODS FOR THE RHETORIC BANK TO GET OUT OF THE MARKET

  ifelse Speed_of_learning = "No learning"

  [set rhetoric_duration  ticks + 1]

  [
    if  found < 20  ;; the rhetoric bank haven't reached O "net" clients for 20 consecutive periods. Why ? : in some simulations, the number of net clients reaches 0 in a period but goes up again in the following period.

  [

  ifelse ( count (consumers with [client = "2" and unhappy = "0"]) <= 1)

    [

      set found found + 1

      if found = 20
      [

      set rhetoric_duration  ticks - 20

      ]

      ]

    [
      set found 0
    ]
  ]
  ]

  end
@#$#@#$#@
GRAPHICS-WINDOW
185
12
728
576
20
20
13.0
1
10
1
1
1
0
0
0
1
-20
20
-20
20
0
0
1
ticks
30.0

BUTTON
7
13
70
46
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
74
14
137
47
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
5
126
177
159
P
P
0.001
0.01
0.001
0.001
1
NIL
HORIZONTAL

SLIDER
5
163
177
196
q
q
0.1
0.7
0.3
0.1
1
NIL
HORIZONTAL

SLIDER
5
89
177
122
distance_banks
distance_banks
0
10
10
1
1
NIL
HORIZONTAL

SLIDER
5
53
178
86
threshold
threshold
1
8
2
1
1
NIL
HORIZONTAL

SLIDER
4
200
177
233
Noise
Noise
0
3
0
0.1
1
NIL
HORIZONTAL

PLOT
727
12
914
240
Ethics satisfaction (+ noise)
Ethics score (perception)
NIL
0.0
10.0
0.0
1.0
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "set-plot-x-range 0 10\nset-plot-pen-color red\nset s 0\nwhile [s <= noise]\n[\nlet my-set fuzzy:gaussian-set (list 10 (2 + s - (noise / 2)) [0 10])\nfuzzy:plot my-set\nset s s + 0.125\n]"

PLOT
916
12
1106
240
Ethics non satisfaction (+ noise)
Ethics score (perception)
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "set-plot-x-range 0 10\nset-plot-pen-color blue\nset s 0\nwhile [s <= noise]\n[\nlet my-set fuzzy:gaussian-set (list 0 (2 + s - (noise / 2)) [0 10])\nfuzzy:plot my-set\nset s s + 0.125\n]"

PLOT
1107
12
1290
239
Likely and unlikely choice
Bank's aggregated score
NIL
0.0
10.0
0.0
1.0
false
false
"" ""
PENS
"Likely" 1.0 0 -13345367 true "" "fuzzy:plot fuzzy:gaussian-set [10 1 [0 10]]"
"Unlikely" 1.0 0 -2674135 true "" "fuzzy:plot fuzzy:gaussian-set [0 1 [0 10]]"

PLOT
1289
12
1482
240
Very likley and unlikely choices
Bank's aggregated score
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"Very unlikely" 1.0 0 -2674135 true "" "fuzzy:plot fuzzy:power fuzzy:gaussian-set [0 1 [0 10]] 2"
"Very likely" 1.0 0 -13345367 true "" "fuzzy:plot fuzzy:power fuzzy:gaussian-set [10 1 [0 10]] 2"

TEXTBOX
19
436
186
580
**LEGEND**\n- Black star : Rhetoric bank\n- White star : Ethical bank\n- Green Box : Conventional bank's client\n- Black Box : Rhetoric bank's client\n- White box : Ethical bank's client\n- Pink box : Unhappy Rhetoric bank's client (through learning)\n- Yellow box : Unhappy Rhetoric bank's client (through peer effect)
10
0.0
0

PLOT
728
240
1107
572
Number of clients
Periods
Clients
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Rhetoric bank (net)" 1.0 0 -16777216 true "" "plot count (consumers with [client = \"2\" and unhappy = \"0\"])"
"Rhetoric bank unhappy" 1.0 0 -2674135 true "" "plot count (consumers with [unhappy = \"1\"])"
"Ethical bank" 1.0 0 -10899396 true "" "plot count (consumers with [client = \"1\"])"

MONITOR
1172
270
1377
315
Rhetoric duration (number of periods)
rhetoric_duration
17
1
11

CHOOSER
6
332
180
377
type-of-aggregation
type-of-aggregation
"sum" "max" "prob-or"
1

CHOOSER
6
379
179
424
type-of-defuzzification
type-of-defuzzification
"COG" "FOM" "LOM" "MOM" "MeOM"
0

CHOOSER
7
275
180
320
Speed_of_learning
Speed_of_learning
"No learning" "Regular" "Fast" "Slow"
1

MONITOR
1173
455
1376
500
Ethical bank % of clients
100 * count (consumers with [client = \"1\"]) / 1681
0
1
11

MONITOR
1173
404
1376
449
Rhetoric bank % of clients (net)
100 * count (consumers with [client = \"2\" and unhappy = \"0\"]) / 1681
0
1
11

MONITOR
1173
508
1378
553
% of unhappy clients @Rhetoric bank
100 * count (consumers with [unhappy = \"1\"]) / 1681
0
1
11

MONITOR
1173
349
1376
394
Conventional Bank % of  clients
100 * (1681 - count (consumers with [client = \"1\"]) - count (consumers with [client = \"2\"]) )/ 1681
0
1
11

TEXTBOX
14
238
181
282
Simulation is slower when noise is high
11
15.0
1

@#$#@#$#@
## WHAT IS IT?

The model allows simulating the dynamics of a banking market in a multi-agent environment. the simulation tool features two Islamic banks pursuing CSR strategies to attract the conventional bank’s customers that are sensitive to ethics and social orientation. The first Islamic bank (the ethical bank) is genuinely involved in CSR. The second Islamic bank pretends to undertake genuine CSR activities but does not honor its commitment in order to save costs (the Rhetoric bank). The latter bank takes advantage of information asymmetry in the market. As far as consumers are concerned, the proposed agent based model reflects realistic human behavioral factors relating to product adoption process: Heterogeneity of consumers, peer influence, and fuzzy decision-making based on multiple attributes. The simulation was coded and tested on Netlogo 5.0.4 version.


## FUZZY LOGIC EXTENSION

This simulation relies on the fuzzy-logic-extension-model version 1.1 developed by Doina Olaru & Luis R. Izquierdo. fuzzy-logic-extension-model is a model designed to facilitate the implemention of fuzzy IF-THEN rules in NetLogo.

The fuzzy-logic-extension file "Fuzzy.zip" has to be unzipped and placed in the same directory as the Netlogo file


## SIMULATION INPUTS

•	Threshold in the range [1,5]. Used in the contagion model (peer effect)
•	p in the range [0.001, 0.01] and q in the range [0, 0.7]. Used in the consumers’ selection model
•	Noise in the range [0,3]. Used to differentiate between consumers preferences for ethics
•	Speed of learning takes three values (slow, regular or fast). Used for rhetoric bank’s consumers
•	Type of aggregation takes the values “Sum” and “Max”
•	Type of defuzzification takes the values "COG", "FOM", "LOM", "MOM" and "MeOM"


## SIMULATION GRID

•	1681 consumers are either clients of the Ethical bank (white boxes), the rhetoric bank (black boxes) or the conventional bank (green boxes)
•	Unhappy rhetoric bank’s clients through learning and contagion are represented by yellow boxes and pink boxes respectively
•	For a better view of the simulation interface, go to the “Zoom” menu (in the toolbar) and click on “smaller”


## SIMULATION OUTPUTS

•	Conventional bank : Number of clients
•	Ethical bank : Number of clients
•	Rhetoric bank :
	o	Net number of clients excluding unhappy clients
	o	Number of unhappy clients
	o	Duration : Number of periods for the bank to get out of the market



## SPEEDING THE SIMULATION

The Fuzzy logic extension uses CPU and memory, to speed the simulation click on “Faster speed” on in the Netlogo interface. Please not that simulation is slower when “noise” is high


## MAIN FINDINGS

The simulation results show that the rhetoric bank can thrive in markets characterized by large information asymmetry, by clients with heterogeneous ethical behavior and by the absence of a close competing bank with a genuine ethical positioning. However, such a bank will end-up out of the market when information asymmetry is reduced through learning and when consumers influence their peers’ attitudes


## REFERENCES

IZQUIERDO, L. R., OLARU, D., IZQUIERDO, S. S., PURCHASE, S. & SOUTAR, G. N. 2015. Fuzzy Logic for Social Simulation using NetLogo. Journal of Artificial Societies and Social Simulation.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="distance_population" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="35"/>
    <metric>tipping_point</metric>
    <steppedValueSet variable="distance_banks" first="0" step="1" last="10"/>
    <steppedValueSet variable="population_ethics" first="30" step="2" last="60"/>
  </experiment>
  <experiment name="distance" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1"/>
    <metric>count (consumers with [client = "2"])</metric>
    <metric>count (consumers with [client = "1"])</metric>
    <steppedValueSet variable="distance_banks" first="0" step="1" last="10"/>
  </experiment>
  <experiment name="Noise" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1"/>
    <metric>count (consumers with [client = "2"])</metric>
    <metric>count (consumers with [client = "1"])</metric>
    <steppedValueSet variable="Noise" first="0" step="0.25" last="3"/>
  </experiment>
  <experiment name="Noise distance" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1"/>
    <metric>count (consumers with [client = "1"])</metric>
    <steppedValueSet variable="Noise" first="0" step="0.25" last="3"/>
    <steppedValueSet variable="distance_banks" first="0" step="1" last="10"/>
  </experiment>
  <experiment name="rhetoric_duration" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1"/>
    <metric>rhetoric_duration</metric>
    <steppedValueSet variable="Noise" first="0" step="0.25" last="3"/>
    <steppedValueSet variable="distance_banks" first="0" step="1" last="10"/>
  </experiment>
  <experiment name="threshold_distance" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1"/>
    <metric>rhetoric_duration</metric>
    <enumeratedValueSet variable="threshold">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="distance_banks" first="0" step="1" last="10"/>
  </experiment>
  <experiment name="Noise threshold" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1"/>
    <metric>rhetoric_duration</metric>
    <steppedValueSet variable="Noise" first="0" step="0.25" last="3"/>
    <enumeratedValueSet variable="threshold">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PQ" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1"/>
    <metric>rhetoric_duration</metric>
    <steppedValueSet variable="P" first="0.001" step="0.001" last="0.01"/>
    <steppedValueSet variable="Q" first="0.1" step="0.05" last="0.5"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
