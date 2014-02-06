{-# LANGUAGE ViewPatterns #-}

module Test where
import System.Random

import Types
import Utils

data Question = Ques Int (Maybe ComponentProperty) (Maybe ComponentProperty)
data ComponentProperty = VoltageVal Voltage 
                        | CurrentVal Current
                        | ResistanceVal (Resistance' Int)


sourceToQuestion :: Source -> Question
sourceToQuestion (VoltageSource val) = Ques 0 (Just $ VoltageVal val) Nothing
sourceToQuestion (CurrentSource val) = Ques 0 (Just $ CurrentVal val) Nothing


--always show source, althought this isn't strictly necessary
--add instances for nodes, trivial but it's complete then
netzToQuestions :: Source -> CompTree -> Int -> StdGen -> [Question]
netzToQuestions src@(VoltageSource _) tree@(RoseTree (Block par _ _ _ _ _) _) cnt initGen = 
    let (showing, g1) = (True,initGen)-- random g :: (Bool, StdGen)
        (curStat, volStat) = if par
                                then (True,  True)-- curSafe, volCovered: 
                                             -- we have a non current property
                                             -- and we have one voltage already
                                else (False, False)-- curCovered, volSafe:
                                             -- we don't have a current yet
                                             -- we don't have a non voltage yet
        (_,_,allQuestions,_) = treeToQuestions tree cnt (curStat, volStat) g1
        addSource questions = if showing then sourceToQuestion src:questions
                                         else questions
    in addSource allQuestions

netzToQuestions src@(CurrentSource _) tree@(RoseTree (Block par _ _ _ _ _) _) cnt initGen = 
    let (showing, g1) = (True,initGen)-- random g :: (Bool, StdGen)
        (curStat, volStat) = if par
                                then (False,  False)-- curSafe, volCovered: 
                                             -- we don't have a non currrent yet
                                             -- we don't have a voltage yet
                                else (True, True)-- curCovered, volSafe:
                                             -- we have a current already
                                             -- we have a non voltage already
        (_,_,allQuestions,_) = treeToQuestions tree cnt (curStat, volStat) g1
        addSource questions = if showing then sourceToQuestion src:questions
                                         else questions
    in addSource allQuestions

treeToQuestions :: CompTree -> Int -> (Bool, Bool) -> StdGen -> (Int, (Bool, Bool), [Question], StdGen)
treeToQuestions b@(size -> 1) count' covered initGen = (count', covered, [], initGen)
treeToQuestions b@(RoseTree (parallel -> False) children) count' covered initGen = 
    foldl findValues (count', covered, [], initGen) children
    -- curCovered - do we have an element that gives the current
    -- volSafe - do we have any elements NOT giving voltage
    where findValues (count, (curCovered', volSafe'), qs, thisGen) (RoseNode props) = 
            let (whichUnknown, g1) = randomR (0,1) thisGen :: (Int, StdGen)
                maxAllow = if curCovered'
                            then 1
                            else 2
                minAllow = if isLast && not volSafe'
                            then 1
                            else 0
                (whichKnown, g2) = randomR (minAllow,maxAllow) g1 :: (Int, StdGen)
                curCovered = if whichKnown == 2 then True
                                        else curCovered'
                volSafe = if whichKnown /= 0 then True
                                        else volSafe'
                shown = Just $ theseLabels !! whichKnown
                question = Just $ remove whichKnown theseLabels !! whichUnknown
                theseLabels = labelChoices props
                isLast = count + 1 - count' == (size b)
            in
            (count+1, (curCovered, volSafe), (Ques count shown question):qs, g2)

          --should never happen, leaving non-exhaustive
          {- findValues (count, covered', qs, g) subb@(RoseTree (parallel -> False) _) = 
            --just pass the covered info oonto the subblock which is also serial
            let (newcount,_,newqs,g1) = toQuestions' subb count covered' g
                in (newcount, covered', newqs++qs, g1) -}

          findValues (count, (curCovered', volSafe'), qs, thisGen) subb@(RoseTree (parallel -> True) _)= 
          -- if it's the last and we have so far only voltages
          -- we can always get currents from res or vol
          -- can't give any voltages anymore
          --    so we can say voltageCovered = true
          -- TODO for returning coverage?
            let tellVolCovered = isLast && not volSafe'
                --below isn't safe yet, we can allow all currents
                --(but we have to see if all currents were actually given): 
                --   tellCurSafe = not curCovered'
                isLast = count + (size subb) - count' == (size b)
                (newcount,_,newqs,g1) = treeToQuestions subb count (False,tellVolCovered) thisGen
                in (newcount, (curCovered', volSafe'), newqs++qs, g1)

          labelChoices block =
              [VoltageVal $ rVol block,
               ResistanceVal $ rRes block,
               CurrentVal $ rCur block]

treeToQuestions topBlock@(RoseTree (parallel -> True) childs) count' covered initGen = 
    foldl findValues (count',covered,[],initGen) childs
    -- curCovered - do we have an element that gives the current
    -- volSafe - do we have any elements NOT giving voltage
    where findValues (count, (curSafe', volCovered'), qs, thisGen) (RoseNode props) = 
            let (whichUnknown, g1) = randomR (0,1) thisGen :: (Int, StdGen)
                maxAllow = if volCovered'
                            then 1
                            else 2
                minAllow = if isLast && not curSafe'
                            then 1
                            else 0
                (whichKnown, g2) = randomR (minAllow,maxAllow) g1 :: (Int, StdGen)
                volCovered = if whichKnown == 2
                            then True
                            else volCovered'
                curSafe = if whichKnown /= 0
                        then True
                        else curSafe'
                shown = Just $ theseLabels !! whichKnown
                question = Just $ remove whichKnown theseLabels !! whichUnknown
                theseLabels = labelChoices props 
                isLast = count + 1 - count' == (size topBlock)
            in
            (count+1, (curSafe, volCovered), (Ques count shown question):qs, g2)

          findValues (count, (curSafe', volCovered'), qs, thisGen) subb@(RoseTree (parallel -> False) _)= 
          --the next two cases are analog to the toQuestions' case for Serial blocks
            let tellCurCovered = isLast && not curSafe'
                isLast = count + (size subb) - count' == (size topBlock)
                (newcount,_,newqs,g1) = treeToQuestions subb count (tellCurCovered,False) thisGen
                in (newcount, (curSafe', volCovered'), newqs++qs, g1)

          --should never happen, leaving non-exhaustive
          {-findValues (count, covered', qs, thisGen) b@(RoseTree (parallel -> True) _) = 
            let (newcount,covered,newqs,g1) = treeToQuestions b count covered' thisGen 
                in (newcount, covered, newqs++qs, g1)-}

          labelChoices block =
              [CurrentVal $ rCur block,
               ResistanceVal $ rRes block,
               VoltageVal $ rVol block]
