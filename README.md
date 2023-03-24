# "Shared space Traffic" - Coexistence problem

Shared space is an umbrella term which refers to that part of urban road space which all road users (including pedestrians, cyclists, vehicles and disabled people) are encouraged to legally occupy, with little physical or visual separation. It intends to create safer road environments, by adding more danger. This concept is based on the risk homeostasis theory, where humans shift the balance of risk according to the surrounding environment, so that they can coexist. Since shared space is a controversial concept that increase the number and the complexity of interactions among road users, **there are intriguing research questions/hypothesis that have not been answered/tested so far**. 

This repository contains tools to analyze traffic operations and therefore COEXISTENCE in shared space road environments. The analysis is based on **two datasets** which are publicly available with doi:
1) Batista, M., Trifunović, A., Friedrich, B., (2022). Road users’ trajectories in different shared space schemes. https://doi.org/10.24355/dbbs.084-202210281217-0
2) Tzouras, P., Kepaptsoglou, K., Vlahogianni, E. I.(2023). Traffic measurements in Amalias Street. Conventional Road vs Shared Space. Mendeley Data, V2. https://doi.org/10.17632/n3wzjd54pj.2

The data analysis tools that are included in this repository test the following **six hypotheses**:
- **H1**: Shared space leads to higher pedestrian crossing rates at any point, i.e., crossings per pedestrian in the road environment.
- **H2**: Vehicles and pedestrians follow similar paths in shared space.
- **H3**: Compared to a conventional road environment, pedestrian crossings in shared space strongly reduce vehicle speeds.
- **H4**: When the vehicle flow tends to capacity in shared space, there is no difference with a conventional road environment.
- **H5**: As shared space is more an umbrella term, the COEXISTENCE of road users is still influenced by the (informal) design of the road.
- **H6**: COEXISTENCE IS A FEASIBLE SCENARIO! Shared space can create road environments that are less car-dominated and more human-oriented.

In this analysis, the following study **four cases** are considered:

a) [Frankfurter Street](https://www.openstreetmap.org/#map=18/52.10746/8.16162) is located in Bad Rothenfelde, Germany, and was designed as an “encounter zone”. The design includes a continuous surface with different paving designating pedestrian zones along both sides of the street. The road is two-way with a circulation zone of 6.0 m. In the target area, the width of the activity zone does not exceed the 3.5 m; in the mid-block section, this space is also used as a safe zone for pedestrians’ movements.

b) [Lange Street](https://www.openstreetmap.org/#map=18/52.16817/9.24980) is located in Hessisch Oldendorf, Germany, and was designed as a “traffic calming zone". The design includes a continuous surface with different paving designating pedestrian zones along both sides of the street. This case refers to a one-way street with a circulation zone of only 3.0 m. Focusing on the target zone where measurements are collected, the pedestrian space (i.e., activity and circulation zone) in each side is 3.0 meters.

c) [Marktplatz](https://www.openstreetmap.org/#map=18/52.25022/10.81891) is located in Königslutter am Elm, Germany, and was designed as a “20 km/h zone. Different paving visually segregates the walking and driving zones and a slight level of segregation is presented in the pedestrian zone. The target area of Marktplatz includes a square of 14 m. wide; this is considered as an activity zone. On the other side, the safe zone that is utilized by pedestrians is approximately 3.5 m. The road is two-way with a circulation zone of 6 m.

d) [Amalias Street](https://www.openstreetmap.org/#map=19/37.56608/22.79967) is located in Nafplio, Greece, and serves as an access street for vehicles to the pedestrianized (historical) city center. It contains two main sections, one with a conventional road design and the other with a shared space design. In the shared space section, the width of the circulation decreases from 7 m to 4.25 m, while the sum of activity and safe zone increases from 3.5 m to 6.25 m.

<img width="550" alt="image" src="https://user-images.githubusercontent.com/63541107/227502726-1de18f10-35a4-49e1-81ed-29969812e162.png">

**The [Shared_space_Traffic](https://github.com/panosgjuras/Shared_space_Traffic) repository contains:**
- [lsaxony_procss](https://github.com/panosgjuras/Shared_space_Traffic/tree/main/lsaxony_procss): contains tools programmed in python to plot trajectories (!!) and export traffic measurements from the first three cases (a, b and c).
- [amalias_procss](https://github.com/panosgjuras/Shared_space_Traffic/tree/main/amalias_procss) is a preliminary analysis of data collected in Amalias Str. Statistical packages in R-programming language are utilized in this analysis. The exported figures and models show the major differences between shared space and conventional road section.
- [comb_procss](https://github.com/panosgjuras/Shared_space_Traffic/tree/main/comb_procss) data processing and analysis of the four study cases. Statistical packages in R-programming language are utilized in this analysis. The output models and plots provide some usefull answers to the coexistence problem.

Since 2021, the following **papers** have been published.
1. Batista, M., & Friedrich, B. (2022). Investigating spatial behaviour in different types of shared space. Transportation Research Procedia, 60(2021), 44–51. https://doi.org/10.1016/j.trpro.2021.12.007
2. Tzouras, P. G., Karolemeas, C., Kepaptsoglou, K., & Vlahogianni, E. I. (2022). Towards the Estimation of Macroscopic Traffic Parameters in Shared Space Network Links: An Empirical Study. 101st Annual Meeting of the Transportation Research Board (TRB). https://trid.trb.org/view/1909535
