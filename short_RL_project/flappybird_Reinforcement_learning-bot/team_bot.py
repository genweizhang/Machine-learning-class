import json
from os.path import abspath

# This bot is made for CS 4033

class Team_Bot:
    def __init__(self):
        self.Q = {}
        self.states = ()            # string tuple (x_section, y_section, velocity)
        self.rewards = {'alive': 1, 'dead': -100}
        self.alpha = 0.5           # learning rate, can change value
        self.gamma = 0.9            # for some reason this is 1 instead of 0.9
        self.previousState = (400, 300, 2)  # initial value for bird
        self.previousAction = 0     # initially waiting
        self.pastExperiences = []
        self.gamesPlayed = 0
        self.pullfromExperience()


    def pullfromExperience(self):
        readFile = open(abspath("newteamqvalues.json"), 'r')
        self.Q = json.load(readFile)
        readFile.close()

    
    def act(self, x_distance, y_distance, velocity):
        # returns index of action to be used [0 = wait, 1 = flap]    
        # keep track of [s_t, a_t, s_t+1] (insert at beginning)
    
        state = self.mapState(x_distance, y_distance, velocity)

        self.pastExperiences.insert(0, [self.previousState, self.previousAction, state])

        # get index of max q-value (or 0 if tie)
        if self.Q[str(state)][0] == self.Q[str(state)][1]:
            actionIndex = 0
        else:
            actionIndex = self.Q[str(state)].index(max(self.Q[str(state)]))

            
        # update
        self.previousState = state
        self.previousAction = actionIndex

        return actionIndex


    def mapState(self, x_distance, y_distance, velocity):
        # returns a tuple representing state
        # make sure that states are discrete
        x_section = int(x_distance - x_distance%20)
        y_section = int(y_distance - y_distance%15)

        if x_distance < 0:
            x_section = int(x_distance + abs(x_distance)%20)
        if y_distance < 0:
            y_section = int(y_distance + abs(y_distance)%15)

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

            # apply Q-learning
            self.Q[str(s)][a] += self.alpha * (reward + self.gamma * max(self.Q[str(s1)]) - self.Q[str(s)][a])

        self.gamesPlayed += 1

        # update local q-values file if 5 games have been played
        if self.gamesPlayed % 1 == 0:
                writeFile = open(abspath("newteamqvalues.json"), 'w')    
                json.dump(self.Q, writeFile)
                writeFile.close()
        
        
        # clear past experiences
        self.pastExperiences = []
        