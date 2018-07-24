import json
import math

class LogStateBot:
    def __init__(self):
        self.Q = {}
        self.states = ()                    # string tuple (x_section, y_section, velocity)
        self.rewards = {'alive': 1, 'dead': -100}
        self.alpha = 0.5                    # learning rate, can change value
        self.gamma = 0.9                    # for some reason this is 1 instead of 0.9
        self.previousState = (420, 243, 0)  # initial state 240
        self.previousAction = 0             # initally waiting
        self.pastExperiences = []
        self.gamesPlayed = 0
        self.xStates = []
        self.yStates = []
        self.exp_rate = -0.18

        self.initializeStates() # maps all possible states
        self.pullfromExperience()


    def initializeStates(self):
        x = 420
        y = -300
        # add x and -x (symmetric) also put positive x into y
        while x >= 20:
            (self.xStates).append(x)
            (self.xStates).append(-x)

            self.yStates.append(x)

            x = (int)(x*math.exp(self.exp_rate))
        
        while y <= -20:
            self.yStates.append(y)
            y = (int)(y*math.exp(self.exp_rate))

        (self.xStates).sort()
        (self.yStates).sort()


    def pullfromExperience(self):
        readFile = open('../logQValues.json', 'r')
        self.Q = json.load(readFile)
        readFile.close()


    # need an act method
    # returns index of action used [0 = wait, 1 = flap]
    def act(self, x_distance, y_distance, velocity):
        
        # determine current state
        state = self.mapState(x_distance, y_distance, velocity)

        # keep track of [s_t, a_t, s_t+1]
        self.pastExperiences.insert(0, [self.previousState, self.previousAction, state])

       # if tie, choose not to flap (only worry about bottom pipes)
        if self.Q[str(state)][0] == self.Q[str(state)][1]:
            actionIndex = 0
        else:
            actionIndex = self.Q[str(state)].index(max(self.Q[str(state)]))

        # update previous state
        self.previousState = state

        # update previous action
        self.previousAction = actionIndex

        return actionIndex


    # returns a tuple representing state
    def mapState(self, x_distance, y_distance, velocity):

        xDiff = []
        yDiff = []

        for x in self.xStates:
            xDiff.append(x - x_distance)

        for y in self.yStates:
            yDiff.append(y - y_distance)

        # find min distance in x/y states
        absXDistance = [abs(x) for x in xDiff]
        absYDistance = [abs(y) for y in yDiff]
        x_section = (int) (x_distance + xDiff[absXDistance.index(min(absXDistance))])
        y_section = (int) (y_distance + yDiff[absYDistance.index(min(absYDistance))])

        return (x_section, y_section, velocity)

   
   
    def update_scores(self):
        # apply q-learning method on past state-action pairs

        for i in range(0, len(self.pastExperiences)):

            # experience = [s, a, s'] 
            experience = self.pastExperiences[i]

            s = experience[0]
            a = experience[1]
            s1 = experience[2]

            # decide reward 
            # bird will only be dead on the last move (first index)
            if i == 0 :
                reward = self.rewards['dead']
            else:
                reward = self.rewards['alive']

            # apply q learning
            self.Q[str(s)][a] += self.alpha * (reward + self.gamma * max(self.Q[str(s1)]) - self.Q[str(s)][a])

        self.gamesPlayed += 1

        # update local q-values file if 5 games have been played
        if self.gamesPlayed % 5 == 0:
                writeFile = open('../logQValues.json', 'w')    
                json.dump(self.Q, writeFile)
                writeFile.close()
        
        
        # clear past experiences
        self.pastExperiences = []