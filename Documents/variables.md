# Derived Variable Formulas

This document provides the mathematical formulas and definitions for the newly created **Ratio**, **WAB** (Wins Above Bubble), and **Qualify** variables in the model.

## 1. Win/Loss Ratio Variables

The ratio variables are calculated by dividing a team's wins by their losses plus one. The "+ 1" prevents division-by-zero errors for undefeated teams. 

**Formula:**
$$ R_{c} = \frac{W_{c}}{L_{c} + 1} $$

Where subscript $c$ represents the specific game category (e.g., Overall, Conference, Road).

## 2. Wins Above Bubble (WAB)

Wins Above Bubble (WAB) estimates how many more wins a team achieved compared to an average "bubble" team playing the exact same schedule. A bubble team is defined as having a NET Rating of 85 ($R_{bubble} = 85$).

First, a baseline logistic regression model estimates the probability of the home team winning based on the difference in ratings:
$$ P(\text{Home Win}) = \sigma(\beta_0 + \beta_1(Rating_{away} - Rating_{home})) $$

For each game, we compute the expectation if a bubble team ($R_{bubble} = 85$) took the place of the actual team:
- **Home Bubble Win Probability:** $P_{(bubble, home)}$ is the expected win probability of a bubble team playing at home against the actual away opponent.
- **Away Bubble Win Probability:** $P_{(bubble, away)}$ is the expected probability used for a bubble team playing away against the actual home opponent.

The WAB for a single game is the actual game outcome ($O_{game} \in \{0, 1\}$) minus the expected probability from the bubble model.

**Total WAB:**
$$ \text{WAB}_{total} = \sum_{i \in \text{Home Games}} (O_{home, i} - P_{(bubble, home)_i}) + \sum_{j \in \text{Away Games}} (O_{away, j} - P_{(bubble, away)_j}) $$

*(Note: Based on the `FinalModel.R` implementation, WAB is calculated independently for home and away subsets and then aggregated across the season).*

---

## 3. Tournament Qualification (Qualify)

The qualification variable ($Q$) acts as a binary flag indicating whether a team makes the tournament. The logic differs between the historical (training) dataset and the future (testing) dataset.

**Historical Data (Training):**
A team is marked as qualified if they received any type of tournament bid (`Bid.Type` is not NA).
$$ Q_{train} = \begin{cases} 1 & \text{if Bid.Type } \neq \text{NA} \\ 0 & \text{otherwise} \end{cases} $$

**Future Data (Testing):**
1. An XGBoost classification model predicts each team's probability of qualifying ($P_{qualify}$) based on their resume metrics.
2. Within each season, all teams are sorted by their predicted probability in descending order to assign a qualification rank ($Rank_{qualify}$). Ties are broken sequentially based on the team's first appearance in the dataset.
3. The top 68 teams are assigned a qualification status of 1.

$$ Q_{test} = \begin{cases} 1 & \text{if } Rank_{qualify} \le 68 \\ 0 & \text{otherwise} \end{cases} $$

---

## Variable Definitions

| Code Variable | Mathematical Symbol | Description |
| :--- | :---: | :--- |
| `WL.Ratio` | $R_{wl}$ | Overall Win/Loss Ratio |
| `Conf.Ratio` | $R_{conf}$ | Conference Win/Loss Ratio |
| `NonConf.Ratio` | $R_{nonconf}$ | Non-Conference Win/Loss Ratio |
| `Road.Ratio` | $R_{road}$ | Road Games Win/Loss Ratio |
| `Q1.Ratio` | $R_{q1}$ | Quadrant 1 Win/Loss Ratio |
| `Q2.Ratio` | $R_{q2}$ | Quadrant 2 Win/Loss Ratio |
| `Q3.Ratio` | $R_{q3}$ | Quadrant 3 Win/Loss Ratio |
| `Q4.Ratio` | $R_{q4}$ | Quadrant 4 Win/Loss Ratio |
| `bubbleWinProb_Home` | $P_{(bubble, home)}$ | Expected probability if a bubble team were the home team |
| `bubbleWinProb_Away` | $P_{(bubble, away)}$ | Expected probability if a bubble team were the away team |
| `WAB_home` | $\text{WAB}_{home}$ | Total Wins Above Bubble accumulated from home games |
| `WAB_away` | $\text{WAB}_{away}$ | Total Wins Above Bubble accumulated from away games |
| `WAB` | $\text{WAB}_{total}$ | A team's total Season Wins Above Bubble |
| `prob_qualify` | $P_{qualify}$ | XGBoost predicted probability of making the tournament |
| `ranks_qualify` | $Rank_{qualify}$ | Team's rank based on predicted qualification probability within a season |
| `Qualify` | $Q$ | Binary tournament qualification status (1 = Qualified, 0 = Did not qualify) |
