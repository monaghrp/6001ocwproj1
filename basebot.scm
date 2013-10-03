;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v 
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a 
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2 
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when 
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(define square
  (lambda (x) (* x x)))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s
(define pi 3.14159)

;; Problem 1

(define position
  (lambda (a v u t)
    (+ (/ (* a t t) 2) (* v t) u)))

;; you need to complete this procedure, then show some test cases

; (position 0 0 0 0)
; (position 0 0 20 0)
; (position 0 5 10 10)
; (position 2 2 2 2)
; (position 5 5 5 5)


;; Problem 2

(define root1
  (lambda (a b c)
    (/ (+ (- 0 b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))))

(define root2
  (lambda (a b c)
    (/ (- (- 0 b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))))

;; complete these procedures and show some test cases

;; Problem 3

(define time-to-impact
  (lambda (vertical-velocity elevation)
	(if (> (root1 (/ -9.8 2) vertical-velocity elevation) 0) (root1 (/ -9.8 2) vertical-velocity elevation) (root2 (/ -9.8 2) vertical-velocity elevation))))

;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (if (> (root1 (/ -9.8 2) vertical-velocity (- elevation target-elevation)) 0)
	(root1 (/ -9.8 2) vertical-velocity (- elevation target-elevation))
	(root2 (/ -9.8 2) vertical-velocity (- elevation target-elevation)))))

;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree2radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

(define travel-distance-simple
  (lambda (elevation velocity angle)
	(* velocity (time-to-impact (* velocity (sin (degree2radian angle))) elevation) (cos (degree2radian angle)))))

;; let's try this out for some example values.  Note that we are going to 
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;; at an angle of (/ pi 4) radians or 45 degrees

;; what is the distance traveled in each case?
;; record both in meters and in feet


;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define alpha-increment 0.01)

(define find-best-angle
  (lambda (velocity elevation)
    (find-best-angle-helper velocity elevation 0 0 0)))


(define find-best-angle-helper (lambda (v u ang bdist bang)
	(if (> ang 90) bang
		(if (> (travel-distance-simple u v (+ ang 0.001)) bdist)
			(find-best-angle-helper v u (+ ang 0.001) (travel-distance-simple u v (+ ang 0.001)) (+ ang 0.001))
			(find-best-angle-helper v u (+ ang 0.001) bdist bang)))))

;; find best angle
;; try for other velocities
;; try for other heights

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin 
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two 
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level 
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4 
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor 
;; that can be computed

;; We would like to again compute distance , but taking into account 
;; drag.
;; Basically we can rework the equations to get four coupled linear 
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the 
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speeed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one 
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define dx (lambda (u dt) (* u dt)))
(define dy (lambda (v dt) (* v dt)))
(define speed (lambda (u v) (sqrt (+ (* u u) (* v v)))))
(define du (lambda (m beta u v dt) (* (/ -1 m) beta (speed u v) u dt)))
(define dv (lambda (m beta u v dt g) (* dt (* -1 (+ g (* (/ 1 m) (speed v u) v beta))))))


(define integrate
  (lambda (x0 y0 u0 v0 dt g m beta)
	(if (< y0 0) x0
		(integrate 
			(+ x0 (dx u0 dt))
			(+ y0 (dy v0 dt))
			(+ u0 (du m beta u0 v0 dt))
			(+ v0 (dv m beta u0 v0 dt g))
			 dt g m beta))))

(define travel-distance (lambda (x0 y0 v dt g m rho ang)
  (integrate x0 y0 (* v (cos ang)) (* v (sin ang)) dt g m rho)))


;; RUN SOME TEST CASES
;;various speeds
(travel-distance 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 45))
;;93.23
(travel-distance 0 0 40 0.01 9.8 0.15 0.001344011 (degree2radian 45))
;;82.31
(travel-distance 0 0 35 0.01 9.8 0.15 0.001344011 (degree2radian 45))
;;70.59

;;boston
;;ball will make it over the fence with angle ~31deg to ~48deg
(travel-distance 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 25))
;;85.98
(travel-distance 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 30))
;;91.17
(travel-distance 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 31))
;;91.98
(travel-distance 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 35))
;;94.03
(travel-distance 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 40))
;;94.71
(travel-distance 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 45))
;;93.23
(travel-distance 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 48))
;;91.44
(travel-distance 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 50))
;;89.8
(travel-distance 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 55))
;;84.48
(travel-distance 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 60))
;;77.19
(travel-distance 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 65))
;;68.17
(travel-distance 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 70))
;;57.41
(travel-distance 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 75))
;;44.97
(travel-distance 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 80))
;;31.08
(travel-distance 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 85))
;;15.94
(travel-distance 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 90))
;;0

;; what about Denver?
;;will make it over the fence from ~25deg to ~55deg
(travel-distance 0 0 45 0.01 9.8 0.15 0.00114 (degree2radian 24))
;;90.19
(travel-distance 0 0 45 0.01 9.8 0.15 0.00114 (degree2radian 25))
;;91.76
(travel-distance 0 0 45 0.01 9.8 0.15 0.00114 (degree2radian 30))
;;97.76
(travel-distance 0 0 45 0.01 9.8 0.15 0.00114 (degree2radian 35))
;;101.21
(travel-distance 0 0 45 0.01 9.8 0.15 0.00114 (degree2radian 40))
;;102.27
(travel-distance 0 0 45 0.01 9.8 0.15 0.00114 (degree2radian 45))
;;100.93
(travel-distance 0 0 45 0.01 9.8 0.15 0.00114 (degree2radian 50))
;;97.32
(travel-distance 0 0 45 0.01 9.8 0.15 0.00114 (degree2radian 55))
;;91.65
(travel-distance 0 0 45 0.01 9.8 0.15 0.00114 (degree2radian 60))
;;83.8
(travel-distance 0 0 45 0.01 9.8 0.15 0.00114 (degree2radian 65))
;;74.05
(travel-distance 0 0 45 0.01 9.8 0.15 0.00114 (degree2radian 70))
;;62.38
(travel-distance 0 0 45 0.01 9.8 0.15 0.00114 (degree2radian 75))
;;48.84
(travel-distance 0 0 45 0.01 9.8 0.15 0.00114 (degree2radian 80))
;;33.76
(travel-distance 0 0 45 0.01 9.8 0.15 0.00114 (degree2radian 85))
;;17.29
(travel-distance 0 0 45 0.01 9.8 0.15 0.00114 (degree2radian 90))
;;0

;; Problem 7
 
;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to 
;; use, given a velocity, in order to reach a given height (receiver) at a 
;; given distance

(define bang-throw (lambda (v d ang) 
	(if (> (travel-distance 0 0 v 0.01 9.8 0.15 0.001344011 (degree2radian ang)) d) ang (bang-throw v d (+ ang 0.01)))))


(define integrate-time
  (lambda (x0 y0 u0 v0 dt g m beta t)
	(if (< y0 0) t
		(integrate-time 
			(+ x0 (dx u0 dt))
			(+ y0 (dy v0 dt))
			(+ u0 (du m beta u0 v0 dt))
			(+ v0 (dv m beta u0 v0 dt g))
			 dt g m beta (+ t dt)))))

(define travel-time (lambda (x0 y0 v dt g m rho ang)
  (integrate-time x0 y0 (* v (cos ang)) (* v (sin ang)) dt g m rho 0)))
(travel-time 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 26.63))

;; a cather trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

;;45m/s
(bang-throw 45 36 0)
;;6.19deg
(travel-time 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 6.19))
;;0.95s

;;35m/s
(bang-throw 35 36 0)
;;10.47deg
(travel-time 0 0 35 0.01 9.8 0.15 0.001344011 (degree2radian 10.47))
;;1.23s

;;55m/s
(bang-throw 55 36 0)
;;4.14deg
(travel-time 0 0 55 0.01 9.8 0.15 0.001344011 (degree2radian 4.14))
;;0.78s



;;90mph to home=40.23m/s
;;assume pitchers mound is halfway between home plate and 2nd base 18m
(bang-throw 40.23 18 0)
;;3.38deg
(travel-time 0 0 35 0.01 9.8 0.15 0.001344011 (degree2radian 3.38))
;;0.43s

;;catchers throws at 90mph=40.23m/s
(bang-throw 40.23 36 0)
;;7.82deg
(travel-time 0 0 35 0.01 9.8 0.15 0.001344011 (degree2radian 7.82))
;;0.94s

;;3 seconds to run between 1st and 2nd base
(- 3 (+ .43 .94))
;;1.63s to catch and release to beat runner

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft) 
;; using 45m/s
;;30m @ 45m/s
(bang-throw 45 30 0)
;;4.92deg
(travel-time 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 4.92))
;;0.76s

;;60m @ 45m/s
(bang-throw 45 60 0)
;;12.69deg
(travel-time 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 12.69))
;;1.82s

;;80m @ 45m/s
(bang-throw 45 80 0)
;;21.11deg
(travel-time 0 0 45 0.01 9.8 0.15 0.001344011 (degree2radian 21.11))
;;2.82s



;;30m @ 55m/s
(bang-throw 55 30 0)
;;3.27deg
(travel-time 0 0 55 0.01 9.8 0.15 0.001344011 (degree2radian 3.27))
;;0.62s

;;60m @ 55m/s
(bang-throw 55 60 0)
;;8.26deg
(travel-time 0 0 55 0.01 9.8 0.15 0.001344011 (degree2radian 8.26))
;;1.46s

;;80m @ 55m/s
(bang-throw 55 80 0)
;;13.21deg
(travel-time 0 0 55 0.01 9.8 0.15 0.001344011 (degree2radian 13.21))
;;2.20s




;;30m @ 35m/s
(bang-throw 35 30 0)
;;8.30deg
(travel-time 0 0 35 0.01 9.8 0.15 0.001344011 (degree2radian 8.30))
;;1.00s

;;60m @ 35m/s
(bang-throw 35 60 0)
;;22.89deg
(travel-time 0 0 35 0.01 9.8 0.15 0.001344011 (degree2radian 22.89))
;;2.49s

;;80m @ 35m/s
(bang-throw 35 80 0)
;;can't make it


;; Problem 8
(define bounce-dist (lambda (y0 v ang bounce dist) (display bounce)
	(if (< v 0.1) dist
	(bounce-dist 0 (/ v 2) ang (+ bounce 1) (+ dist (travel-distance 0 y0 v 0.01 9.8 0.15 0.001344011 (degree2radian ang)))))))


;;45m/s 45deg
(bounce-dist 1 45 45 0 0)
;;148.81m 9 bounces
;;45m/s 55deg
(bounce-dist 1 45 35 0 0)
;;147.98m 9 bounces
;;45m/s 35deg
(bounce-dist 1 45 55 0 0)
;;135.8m 9 bounces

;;35m/s 45deg
(bounce-dist 1 35 45 0 0)
;;107.29m, 9 bounces
;;35m/s 55deg
(bounce-dist 1 35 35 0 0)
;;105.77m 9 bounces
;;35m/s 35deg
(bounce-dist 1 35 55 0 0)
;;98.48m 9 bounces

;;55m/s 45deg
(bounce-dist 1 55 45 0 0)
;;188.62m 10 bounces
;;55m/s 55deg
(bounce-dist 1 55 35 0 0)
;;188.99m 10 bounces
;;55m/s 35deg
(bounce-dist 1 55 55 0 0)
;;171.33m 10 bounces

;; Problem 9
(define integrate-speed
  (lambda (x0 y0 u0 v0 dt g m beta)
	(if (< y0 0) (sqrt (+ (square u0) (square v0)))
		(integrate-speed 
			(+ x0 (dx u0 dt))
			(+ y0 (dy v0 dt))
			(+ u0 (du m beta u0 v0 dt))
			(+ v0 (dv m beta u0 v0 dt g))
			 dt g m beta))))

(define v-bounce (lambda (x0 y0 v ang)
	(integrate-speed 0 0 (* v (cos (degree2radian ang))) (* v (sin (degree2radian ang)))  0.01 9.8 0.15 0.001344011)))





(define bounce-dist-v (lambda (y0 v ang bounce dist)
	(if (< v 0.2) (list dist bounce)
	(bounce-dist-v 0 (/ (v-bounce 0 y0 v ang) 2) ang (+ bounce 1) (+ dist (travel-distance 0 y0 v 0.01 9.8 0.15 0.001344011 (degree2radian ang)))))))


;;45 m/s @ 45deg
(bounce-dist-v 0 45 45 0 0)
;;110.71m 9 bounces

;;45 m/s @ 35deg
(bounce-dist-v 0 45 35 0 0)
;;109.51m 8 bounces

;;45 m/s @ 55deg
(bounce-dist-v 0 45 55 0 0)
;;102.19 8 bounces

;;35 m/s @ 45deg
(bounce-dist-v 0 35 45 0 0)
;;85.40m 9 bounces

;;35 m/s @ 35deg
(bounce-dist-v 0 35 35 0 0)
;;83.47m 8 bounces

;;35 m/s @ 55deg
(bounce-dist-v 0 35 55 0 0)
;;79.32m 8 bounces

;;55 m/s @ 45deg
(bounce-dist-v 0 55 45 0 0)
;;132.32m 9 bounces

;;55 m/s @ 35deg
(bounce-dist-v 0 55 35 0 0)
;;132.23m 8 bounces

;;55 m/s @ 55deg
(bounce-dist-v 0 55 55 0 0)
;;121.29m 9 bounces





