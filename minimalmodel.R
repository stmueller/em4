##
##  This is an abstraction of the model reported by
## Halverson & Hornoff "A Minimal Model for Predicting Visual Search
## in Human-Computer Interaction"


##TO USE THIS, you need to add targets to the visual display.
## each target will have a location (measured in visual degrees),
## a size (also in visual degrees), and a label for identifying.
##
##

## In this model, you need to specify the similarity of each object
## to the target.  This similarity should be specified (1) foveally;
## (2) parafoveally; (3) peripherally.  Currently, foveal similarity is all-or-none;
## everything less than 1.0 is treated identically.  For parafovea and periphery
## the values will influence the likelihood of saccading.

## The model will search until it finds an object with similarity
## to the target of 1.0 (i.e., it found something identical to its goal)
## thus, it will not look for label matches directly.


ResetDisplay <- function()
{
    SCREEN <<- data.frame(x=double(),y=double(),width=double(),height=double())
    LABELS <<- as.character("")  ##labels for each target.  This is used for display, and
                                 ##to determine whether the response is correct.


    IDENTIFIED <<- FALSE    ##marks whether each target has been look at; used by the model
                            ##as a memory scheme for judging revisited
    VISITED <<- FALSE       ##marks whether each target has been visited.  Should not be used by
                            ##the model, but is for instrumentation.

    CLASSIFICATION <<- NA ##Marks a classification decision about each location


    ##these indicate, for each object, the available similarity to the search target in each zone.
    ## rules:
    ## foveasim=1 is sufficient to call it target.
    ## foveasim<1; could increase time to discriminate.
    ##
    ## parafoveasim:
    ##   similarity to target available in parafovea.
    ##   may be used to direct next saccade to nearby targets.
    ##
    ## peripherysim: similarity availability
    ##   similarity to target available everywhere.
    ##   for pop-out, could be high (.9)
    ##   for pop-out conditions, no values > .9 indicates no targets exist.
    ##  
    ##  by default, peripherysim should be low if *critical* information is not available
    ##  peripherally.

   
    FOVEASIM <<- FALSE   ##should range 0 to 1.0
    PARAFOVEASIM <<- FALSE   ##should range 0 to 1.0
    PERIPHERYSIM <<- FALSE   ##should range 0 to 1.0

}

## PEBL search task explanation
## target is O or X, white or green, 0,1, or 5 of them.
## foils are 5 to 15 foils
##
##
## white target:
##
## target
##   X    white O has foveal sim of .8
##                  parafov sim .4-.6
##                  periph sim ~ .75
##   O    white O has foveal sim of 1.0 !target
##                  parafov sim .7-.9
##                  periph sim ~ .75
##   X    white X has foveal sim of 1.0  !target
##                  parafov sim of .7-.9
##                  periph sim ~.75
##   O    white X has foveal sim of .8 (not the same)
##                  parafov sim of .4-.6 ##can reject sometimes in peri
##                  periph sim ~.75
##
##   X   white UDGQC has foveal sim of .8
##                  parafov sim of .4-.6  ##can reject sometimes in peri
##                  periph sim of ~.75
##   O   white UDGQC has foveal sim of .9
##                  parafov sim of .7-.9
##                  periph sim of ~.75
##   
##  X/O   ANYTHING  green has foveal sim of .25
##                            parafov sim  .25
##                            periph sim   .25



## Search process:
##  1. look at periph sim of all targets
##    - Is everything < .5?  If so, reject
##    - if not, look at perifov sim.
##      mark as viewed anything with perifov sim < .5 as viewed
##    -is anything left in perifov?  If so, pick one and move to it.
##            if not, pick something unviewed 'out there' based on
##            distance-randomization scheme + similarity-to-target scheme.

##default perisim to .85, which is high enough to require
##direct an eyemovement, but too high to eliminate peripherally.
AddObject <- function(x,y , width,height,
                      label,
                      foveasim=.6,parasim=.85,perisim=.6)
{

    size <- nrow(SCREEN)+1
    SCREEN[size,] <<- NA
    SCREEN[size,1:4] <<- c(x,y,width,height)
    LABELS[size] <<- (label)
    IDENTIFIED[size] <<- FALSE
    VISITED[size] <<- FALSE
    CLASSIFICATION[size] <<- NA
#    MISIDENTIFIED[size] <<- FALSE

    FOVEASIM[size] <<- foveasim
    PARAFOVEASIM[size] <<- parasim
    PERIPHERYSIM[size] <<- perisim
    
}


## Assume a distance to eye of 60 cm;
##
PlotDisplay <- function(target="",screendist=60)
{

    xlim <-range(c(SCREEN$x+SCREEN$width,SCREEN$x-SCREEN$width))
    ylim <-range(c(SCREEN$y+SCREEN$height,SCREEN$y-SCREEN$height))+c(0,1)

    p <-plot(SCREEN$x,SCREEN$y,type="p",pch=16,cex=.4,bty="o",col="grey",
         xlim=xlim,ylim=ylim,xlab="",ylab="",main=target)


    color <- (IDENTIFIED+ 1)

    strengthcol <- grey(.2+(1-floor(FOVEASIM*100)/100)*.3)


    strengthcol[PERIPHERYSIM>.5] <- "yellow"    
    strengthcol[PERIPHERYSIM>.8] <- "orange"
    strengthcol[FOVEASIM>.8] <- "red"
    rect(SCREEN$x-SCREEN$width/2,
         SCREEN$y-SCREEN$height/2,
         SCREEN$x+SCREEN$width/2,
         SCREEN$y+SCREEN$height/2,
         col=strengthcol)





    targetID <- LABELS==target
    
    trows <- SCREEN[targetID,]
    if(length(trows)>0)
        {
            rect(trows$x-trows$width/2,
                 trows$y-trows$height/2,
                 trows$x+trows$width/2,
                 trows$y+trows$height/2,
                 col=c("navy","red","yellow","orange")[color],
                 lwd=4)
        }
    
    text(SCREEN$x,SCREEN$y,LABELS,cex=.8)

    ##add a 1-cm ruler.
    ##
    cmscale <- 2*screendist * tan(.5*pi/180)
    text(mean(xlim),max(ylim)+.1,      paste("1 unit @ ",screendist ," eye distance"))

    arrows(mean(xlim)-cmscale/2,max(ylim)-.5,
           mean(xlim)+cmscale/2,max(ylim)-.5,
                code=3,angle=90,length=.05)

    arrows(mean(xlim),max(ylim)-.5+cmscale/2,
           mean(xlim),max(ylim)-.5-cmscale/2,
           code=3,angle=90,length=.05)
    
    ##
    
}

GetDistancesToFixation <- function(x,y,noise=0)
{
    ##noise is the scaling factor having mean 1 and sd=noise
    sqrt(rowSums(t(t(SCREEN[,1:2]) - c(x,y))^2))*
            rnorm(nrow(SCREEN),mean=1.0,sd=noise)
}



GetScanPath <- function(startx,starty,
                        targetLabel="XXX",
                        eccnoise=.3,           ##eccentricity noise; represents willingness to move to distant target
                        foveasize = 1.01,      ##radius of fovea, in degrees
                        parafoveasize= 3.5,    ##radius of parafovea, in degrees

#                        detectionfailure=.09,  ##probability of a detection failure
#                        misID = 0,             ##probability of mis-identifying a target. You look at
#                                                ##it but think it is something else, so you do not return to it.

                        detectiontime = 250,   ##time needed to detect stimulus
                        rejectiontime = detectiontime,   ##time needed to reject stimulus.
                        responsetime = 100,    ##time needed to make a response (just needed once)
                        visitedDecayChance = 0.0, ##probability the memory of being identified gets remembered on each saccade; permits a recency gradient of return inhibition.
                        mix = 1.0,               ##mix of distance (1) vs. similarity-based (0.0) selection.
                        allowAbsentResponse = TRUE, ##should exhaustive search trigger 'absent'?.  If FALSE, search will continue until time runs out.

                        neighborhoodsize = 0.0,
                        neighborhooddecay = 0.1,
                        plot=FALSE,
                        maxstep=200,           ## maximum steps to search.Sholudnt' be too big because this is the data we reserve
                        maxtime=20000           ## maximum time to search
                        )
{


    parafovthresh <- 0.8
    time <- 0
    fixationtime <- 0
    N <- length(VISITED)

    VISITED[1:N] <<- NA
    IDENTIFIED[1:N] <<- FALSE
    CLASSIFICATION[1:N] <<- NA
    
    labelpath <- c()
    path <- list(c(startx,starty))
    data <- matrix(NA,ncol=4,nrow=maxstep+1)
    data[1,] <- c(0,0,startx,starty)


    lastdirection <- NA
    found <- FALSE
    x <- startx
    y <- starty
    cont <- TRUE
    step <- 1

    ##give baseline to plot on top of for real-time plotting
    if(plot>2)
        {
            PlotDisplay(target=targetLabel)
        }  
    
    while(cont)
        {

            ## at first, randomly decay visited based on the visitedDecayChance parameter.
            ## this will permit forgetting of past target locations.  It is useful when
            ## environments are noisy or dynamic, or there is a strong chance of missing
            ## identification of the target, 


            IDENTIFIED[runif(length(IDENTIFIED))<visitedDecayChance] <<-FALSE


            ##first, look at periph similarity. We couldprobably use just the non-identified
            ##stimuli here; once you investigate all the stimuli whose periphery sim is
            ##greater than .5 you can reject the rest, but that seems sort of like cheating.

            if(all(PERIPHERYSIM[!IDENTIFIED]<.5))
                {
                    ##we can declare the target is not there!
                    cont <- FALSE
                    found <-  FALSE

                    time <- time + rejectiontime
                    CLASSIFICATION[1:N] <<- FALSE
                    
                }else if(any(PERIPHERYSIM>=.999999))
                     {
                         ##if there is anything with a similarity of 1.0 in the periphery
                         ##declare victory and continue.
                         cont <- FALSE
                         found <- TRUE
                         
                         time <- time + detectiontime
                         CLASSIFICATION <<- (PERIPHERYSIM>=.999999)
                     }else{


                         ##get true distances from current fixation to everything
                         distances0 <- GetDistancesToFixation(x,y,noise=0)


                         ## #################
                         ## Parafovea elimination.  Later, we will move to parafoveal objects first.
                         ## ##################

                         inParaFov <- (distances0 < parafoveasize)
                         eliminate <- inParaFov & (PARAFOVEASIM<parafovthresh)

                         IDENTIFIED[eliminate] <<- T
                         CLASSIFICATION[eliminate] <<- F
                              

                         ## ###################
                         ## # Fovea match
                         ## ###################
                       
                         ##periphery has been examined for immediate rejection condition.
                         ## now, let's consider the foveal stimuli.

                         
                         ##mark everything in-sight as identified
                         ##(unless we screw it up) via misidentification
                         inView <- distances0 < foveasize

                         
                         ##Fovea-based matches work like this:
                         ## chance of calling something 'target' depends
                         ## on the FOVEASIM.  False alarms would be produced
                         ## by non-zero foveasim in foils; misses by
                         ## FOVEASIM less than 1.0.
                         IDENTIFIED <<- (IDENTIFIED | inView)

                         ##[which(inView)] <<- TRUE
                         ##mark anything in view as identified

                         ##This is book-keeping keeping track of
                         ##where we haven't looked; it could be wrong.
                         ##Also, it does not keep track of whether something
                         ##is classified as target or foil.


                         ##for current trial, decide if we are looking at any targets
                         ##based on FOVEASIM (permits false alarms and misses)
                         IDed <- inView & (runif(N)<FOVEASIM)
                         
                         ##anything inview can get a classification assigned to it as well.
                         CLASSIFICATION[which(inView)]  <<- IDed[inView]
                         

                         
                         ##now, mark if the search target has been found?
                         ##This will be true if anything inView has been marked IDed
                         
                         found <- any(IDed[inView])
                         
                         ##add the time delay for detection here. Use fixed duration
                         ##regardless of the number of things identified on this step.
                         ##but rejection time may differ from acceptance time.


                    
                         timestep <- ifelse(found,detectiontime,rejectiontime)
                         time <- time + timestep
                         fixationtime <- fixationtime+timestep





                         ## #############################################
                         ## Determine next eye movement, 

                         
                         if(found)
                             {
                                 cont <- 0
                             }


                         
                         ##check if we have identified everything--if so, end; if not
                         ##figure out the next eye move
                         if(cont & !found & all(IDENTIFIED))
                             {
                                 
                                 ##if we don't allow a 'absent' response, then decay
                                 ##a few of the markers so we can continue search.
                                 if(!allowAbsentResponse)
                                     {
                                         print("Resetting because no absent response allowed")

                                         ##reset.
                                         IDENTIFIED <<- rep(F,length(IDENTIFIED))
                                         cont <- TRUE
                                     } 
                             }

                                 
                     

                         
                         if(cont & !all(IDENTIFIED) & !found)
                             {

                                 ## ###################################
                                 ## # move to things in parafovea next.
                                 ## ###################################
                                 

                                 ##Now, determine where to move eye next.
                                 ##try to select parafoveal targets that are
                                 ##similar to the probe.

                                 
                                 indexes <- 1:N

                                 candidates <- indexes[inParaFov &
                                                           (!IDENTIFIED) &
                                                               (PARAFOVEASIM>parafovthresh)]
                                 if(length(candidates)>0)
                                     {

                                         ##this uses distance only to select the next target in the parafovea.
                                         distances <- distances0 * rnorm(length(distances0),mean=1,sd=eccnoise)
                                         nextpos <- candidates[which.min(distances[candidates])]
                                         useDistance <-F

                                         
                                     }else{
                                    useDistance <- T
                                }
                                 
                             }else{
                                 useDistance <- T
                             }

                         if(all(IDENTIFIED))
                             {
                                 cont <- 0
                             }else
                                 if(!found & useDistance)
                                     {
                                         
                                         ##nothing is available parafoveally at this point
                                         
                                         ##otherwise,use the distance strategy.
                                         ##for the moment, don't use periphery similarity to guide search.
                                         ##consider only unvisited
                                         
                                         indexes <- 1:N
                                         indexes2 <- indexes[!IDENTIFIED]

                                         ##scaled around 0.0:
                                         distances <- (distances0) /mean(distances0) * rnorm(length(distances0),mean=1,sd=eccnoise)
                                         
                                         ##incorporate similarity as well. similarity is scaled 0..1, so reverse it,
                                         ##because we want to move to the _most_ similar (biggest number)
                                         
                                         sim <- ((1-PERIPHERYSIM))/mean(1-PERIPHERYSIM)*rnorm(length(PERIPHERYSIM),mean=1,sd=.05)



                                         ##HH used a randomized distance to select the next location.  In this model,
                                         ##because similarity appears to matter, we also may need  to consider similarity to target.
                                         ##and finally, we should consider destinations that will maximize information gained.
                                         ##this means preferring a destination in a region with many unidentified targets, to permit 
                                         ##secondary identification of as much as possible.  In cases, this could include
                                         ##things that might be identified parafoveally, and of course this could depend
                                         ##on task and instructions.
                                         ## here, the compromise is as follows:
                                         ## if 'neighborhoodsize > 0 ', sim is recomputed to be the sum of the similarities
                                         ## of non-identified targets within neighborhoodsize of the target.
                                         if(neighborhoodsize > 0)
                                             {
                                                 
                                                 dists <-  as.matrix(dist(cbind(SCREEN$x,SCREEN$y)))
                                                 inperi <-outer(PERIPHERYSIM>.5,PERIPHERYSIM>.5) ##select only things that are strong in the periphery
                                                 close <-outer(!IDENTIFIED,!IDENTIFIED) & (dists <neighborhoodsize) & inperi
                                                 
                                                 priority <- colSums(close * (sim * (!IDENTIFIED)) *exp(-neighborhooddecay*dists))

                                        #plot(SCREEN$x,SCREEN$y,col=c("black","grey")[(IDENTIFIED+1)],cex=sim)
                                        #mepoints(SCREEN$x,SCREEN$y,col="blue",cex=priority/(mean(priority))/2)
                                                 sim <- 1/priority  ##invert this to match distance direction.

                                             }

                                         
                                         distances2 <- (distances*mix + sim*(1-mix))[!IDENTIFIED]

                                         
                                         nextpos <- indexes2[which.min(distances2)]

                                     }
                     }
#            print(paste("NEXTPOS:" ,nextpos))
            
            ##we may have already decided to end it all.
            ##If not, compute next move time.
            if(cont)
                {

                    movedist <- distances0[nextpos]

                    ##preparation time depends on whether you are making a saccade in the
                    ##same direction as before.  if the same, 50 ms, if not, 150 ms.
                    ##Halverson & Hornoff indicate that this can overlap with the 149 ms waiting time;
                    ##and in natural search it will rarely happen anyway.
                    
                    
                    preptime <- 0
                    
                    
                    ## eye movement time according to EPIC:
                    ##Compute the eye movement time using
                    ##Ocular_processor and Motor_processor parameters as
                    ## (Voluntary_saccade_intercept +   | 0
                    ## Voluntary_saccade_slope * distance)  | 4ms * distance
                    ## * Execution_fluctuation_factor       |  mean 1.0, sd .5,
                    ## Normal distribution. This parameter can be used
                    ## to produce variability  in movement times. 
                    
                           
                    
                    eyemovementtime <- preptime + (0 + 4 * movedist)*rnorm(1,mean=1.0,sd=.5)
                    
                    ##update time needed to move eye
                    time <- time + eyemovementtime
                    
                    fixationtime <- fixationtime +preptime
                    
                    ##update data record:
                    labelpath <- c(labelpath,nextpos)
                    
                    
                    
                    ##mark target as visited
                    VISITED[nextpos] <<- step            
                    
                    x <- SCREEN[nextpos,1]
                    y <- SCREEN[nextpos,2]

                    
                    data[step+1,] <- c(step,time,x,y)
                    
                    step <- step + 1
                }
            
            
            
                    ##also, stop if we have found it:
                    if(found){ cont <- 0}
            
         
            ##If plotlevel 3 or higher,plot the scanpath realtime
            if(plot>2)
                {
                    
                    points(data[-1:0 +step,3:4],type="o",pch=16,col="darkgrey",cex=1.5,lwd=2)
                    xs <- data[step,3] + foveasize * cos(1:15/15*2*pi)
                    ys <- data[step,4] + foveasize * sin(1:15/15*2*pi)
                    polygon(xs,ys,border="black")
                }

            ## Do a last check for time continuing:


 
            cont <- cont & (step < maxstep) & (time<maxtime)

        }  ##end of while(cont) loop

    ##this should be added once--at response!

    time <- time + responsetime

    ##re-plot everything at the end if plot>2
    if(plot>0)
        {
            PlotDisplay(target=targetLabel)
            points(data[,3:4],type="o",pch=16,col="darkgrey",cex=1.5,lwd=2)

            ##drawing foveas can be inefficient; only draw foveas
            ##forplot>2
            if(plot>1)
                {
                    for(i in 1:nrow(data))
                        {
                            xs <- data[i,3] + foveasize * cos(1:10/10*2*pi)
                            ys <- data[i,4] + foveasize * sin(1:10/10*2*pi)
                            
                            polygon(xs,ys,border="black")

                            xs <- data[i,3] + parafoveasize * cos(1:10/10*2*pi)
                            ys <- data[i,4] + parafoveasize * sin(1:10/10*2*pi)
                            polygon(xs,ys,border="grey")

                        }
                }
        }

    ##Now, determine whether we are correct!
    targid <-  LABELS==targetLabel
    if(any(targid))
        {

            ##the response is positive if anything has been classified as target.
            ##in theory, someone could say yes by mis-identifying another target,
            ##but we assume they also need to identify the correct target.  This
            ##should only impact things when there are multiple correct targets.

            tmp <- any(CLASSIFICATION[targid])
            correct <- ifelse(is.na(tmp),F,tmp)
            
            
        }else{

            ##if there is no target, it is only correct if we have exhaustively
            ##searched and nothing has been identified.
            correct <- !any(CLASSIFICATION)
        }


    
    return (list(found, step,labelpath,data[1:step,],fixationtime,time,correct))
  }

##return values:
##found indicates whether a target was called found (positive response)
## step is number of saccades
## labelpath indicates te labels being looked at on each round.



##returns the angle of movement based on dx and dy
GetAngle <- function(dx,dy)
    {
        (atan2(dy,dx)*180/pi ) %% 360
    }

