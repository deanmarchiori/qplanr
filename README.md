# qplanr
Call Centre Resourcing Shiny App

## About this App  

The QplanR tool will calculate the number of call centre agents required to staff a call centre queue using the Erlang-C traffic formula.   

## Assumptions  

The Erlang C traffic model is an analytical model for estimating the performance of telecommunications systems which incorporate queueing. Queuing applications include switchboard operators and ACD Call Centre agents. The model makes the following assumptions:

- Calls are offered randomly and follow a Poisson arrival distribution  

- Users wait if they find the system busy and do not leave the queue  

- Service times follow a negative exponential distribution  

- Users are served in the order of arrival  

- Users are directed to the first available server (agent)  

- Queue sizes are unlimited.  

## Inputs  

- Target GOS: The desired minimum acceptable service level: % calls answered within target answer time  

- Number of Calls per half hour: The expected number of inbound calls to the call centre  

- Ave Call Duration (sec): The average handling time of the inbound calls  

- Target Speed of Answer (sec): The target answer time for all calls  

## Outputs  

The calculator will provide the number of agents required to keep service levels within target based on the user inputs and the assumptions above.  

Using the modelled number of agents, key indicators are estimated such as probability of calls queueing and ASA. A chart is also produced to show the imapct on GOS for over/under resourcing the queue.  

