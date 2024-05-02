

import java.lang.Math;
import java.util.*; 
import java.io.PrintWriter;


class Markov {



//##################################


    public static double getTransProb(int i, int j, int k) {

        if(k==9) {
            double Markov_probabilities[][] = {
                    {0.5, 0.25, 0, 0.25, 0, 0, 0, 0, 0},
                    {0.25, 0.25, 0.25, 0, 0.25, 0, 0, 0, 0},
                    {0, 0.25, 0.5, 0, 0, 0.25, 0, 0, 0},            //3 by 3 matrix probabilities hard coded
                    {0.25, 0, 0, 0.25, 0.25, 0, 0.25, 0, 0},
                    {0, 0.25, 0, 0.25, 0, 0.25, 0, 0.25, 0},
                    {0, 0, 0.25, 0, 0.25, 0.25, 0, 0, 0.25},
                    {0, 0, 0, 0.25, 0, 0, 0.5, 0.25, 0},
                    {0, 0, 0, 0, 0.25, 0, 0.25, 0.25, 0.25},
                    {0, 0, 0, 0, 0, 0.25, 0, 0.25, 0.5}

            };
            return Markov_probabilities[i - 1][j - 1];
        }

        if(k==4){
            double Markov_probabilities[][]={
                    {0.5,0.25,0.25,0},
                    {0.25,0.5,0,0.25},                 //2x2 matrix probabilities hard coded
                    {0.25,0,0.5,0.25},
                    {0,0.25,0.25,0.5}
            };

            return Markov_probabilities[i - 1][j - 1];
        }

        return 0.1;

    }


//##################################


    public static double getSejProb(int s1, int s2, int numStates, double TS) {
        double count[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

        for (int n = 1; n <= 1000; n++) {
            for (int m = 1; m <= TS; m++) {
                double transition_probability = Math.random();


                return transition_probability;

            }
        }
        return numStates;
    }






    //##################################
    public static double getBiasTransProb(int s1, int s2, double[] ssprob) {

    double acceptance_probability;
        acceptance_probability = Math.min(1,ssprob[s2-1]/ssprob[s1-1]);
        double transition_probability=acceptance_probability*ssprob[s1-1];


    return transition_probability;


    }


//##################################


    public static double getContTransProb(int s1, int s2, double[] rates) {
        if (s1 == 1 && s2 == 2) {
            double transition_probability = (rates[0] / (rates[0] + rates[1]));
            return transition_probability;
        } else if (s1 == 1 && s2 == 3) {
            double transition_probability = (rates[1] / (rates[0] + rates[1]));
            return transition_probability;
        } else if (s1 == 2 && s2 == 1) {
            double transition_probability = (rates[2] / (rates[2] + rates[3]));
            return transition_probability;
        } else if (s1 == 2 && s2 == 3) {
            double transition_probability = (rates[3] / (rates[2] + rates[3]));
            return transition_probability;
        } else if (s1 == 3 && s2 == 1) {
            double transition_probability = (rates[4] / (rates[4] + rates[5]));
            return transition_probability;
        } else if (s1 == 3 && s2 == 3) {
            double transition_probability = (rates[5] / (rates[4] + rates[5]));
            return transition_probability;
        } else {
            double transition_probability = 0;
            return transition_probability;
        }


    };





    public static double getContSejProb(int s1, int s2, double[] rates, double TSC) {


        return 0.1;
    }
}