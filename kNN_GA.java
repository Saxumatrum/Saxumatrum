import java.lang.Math;
import java.util.*; 
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;

public class kNN_GA {

     public static int TRAIN_SIZE=400; //no. training patterns
     public static int VAL_SIZE=200; //no. validation patterns
     public static int FEATURE_SIZE=61; //no. of features

     public static double[][] train = new double [TRAIN_SIZE][FEATURE_SIZE]; //data to train 
     public static double[][] val = new double [VAL_SIZE][FEATURE_SIZE]; //validation data
     public static int[] train_label=new int[TRAIN_SIZE]; //actual target/class label for train data
     public static int[] val_label=new int[VAL_SIZE]; //actual target/class label for validation data 

     public static double accuracy = 0.0;
 
  
  public static void main(String[] args) throws IOException {



     System.out.println("Modify the GA function to obtain the best features.");
     System.out.println("Use your optimal k value for kNN that you found earlier");
     System.out.println("DO NOT modify the MAX_POP or MAX_GEN constants or the output file generation");
     
     Load_Data(TRAIN_SIZE, VAL_SIZE, FEATURE_SIZE); //load data
     GA(); //call GA function 
  
  } //end main loop

 
  public static void Sort (double[][] sort_array, final int column_sort) {
      Arrays.sort(sort_array, new Comparator<double[]>() {
        @Override
        public int compare(double[] a, double[] b) {
        if(a[column_sort-1] > b[column_sort-1]) return 1;
        else return -1;
        }
      });
  }
     
 
  public static int Mode(int neigh[]) {
     int modeVal=0;
     int maxCnt=0;
     
     for (int i = 0; i < neigh.length; ++i) {
         int count = 0;
         for (int j = 0; j < neigh.length; ++j) {
            if (neigh[j] == neigh[i]) 
            count=count+1;
         }
         if (count > maxCnt) {
         maxCnt = count;
         modeVal = neigh[i];
         }
     }
  return modeVal;
  }
 
 
  public static void Load_Data(int TRAIN_SIZE, int VAL_SIZE, int FEATURE_SIZE) throws IOException {
     
    String train_file="alco_train_data.txt"; //read training data
    try (Scanner tmp = new Scanner(new File(train_file))) {
      for (int i=0; i<TRAIN_SIZE; i++)
        for (int j=0; j<FEATURE_SIZE; j++)
          if(tmp.hasNextDouble()) 
          train[i][j]=tmp.nextDouble();
    tmp.close();
    }

    String val_file="alco_val_data.txt"; //read validation data
    try (Scanner tmp = new Scanner(new File(val_file))) {
      for (int i=0; i<VAL_SIZE; i++)
        for (int j=0; j<FEATURE_SIZE; j++)
          if(tmp.hasNextDouble()) 
          val[i][j]=tmp.nextDouble();
    tmp.close();
    }

    String train_label_file="alco_train_label.txt"; //read train label
    try (Scanner tmp = new Scanner(new File(train_label_file))) {
        for (int i=0; i<TRAIN_SIZE; i++)
          if(tmp.hasNextInt()) 
          train_label[i]=tmp.nextInt();
    tmp.close();
    }

    String val_label_file="alco_val_label.txt"; //read validation label (to obtain classification accuracy)
    try (Scanner tmp = new Scanner(new File(val_label_file))) {
        for (int i=0; i<VAL_SIZE; i++)
          if(tmp.hasNextInt()) 
          val_label[i]=tmp.nextInt();
    tmp.close();
    }

  }

  
  public static boolean[] GA() { 

    int POP_SIZE=200; //population size - for assignment, see brief for limit
    int MAX_GEN=25; //maximum generation - for assignment, see brief for limit
    boolean[] temp_sol = new boolean[FEATURE_SIZE];
    double[] fitness =new double[POP_SIZE];
    boolean[][] sol = new boolean[POP_SIZE][FEATURE_SIZE]; 
    boolean[][] new_sol = new boolean[POP_SIZE][FEATURE_SIZE]; 
    boolean[] final_sol = new boolean[FEATURE_SIZE]; //final best pop
  
    //create initial population
    for(int j=0; j<POP_SIZE; j++){ 
    int count=0;
       for(int k=0; k<FEATURE_SIZE; k++)
       {
       sol[j][k]= (Math.random()>0.5);
       temp_sol[k]=sol[j][k];
         if (temp_sol[k])
         count++;
       }
    //modify fitness to include both increasing accuracy and minimising features
    fitness[j]=(double) KNN(train, val, train_label, val_label, temp_sol) - (1.0*count/FEATURE_SIZE); 
    //System.out.print(count + " ");
    } 
    //System.out.println();
    
    System.arraycopy(sol, 0, new_sol, 0, sol.length); //copy initial array, new_sol=sol; 
       
    
    for (int gen=0; gen<MAX_GEN; gen++) { //do for many generations
    
      System.arraycopy(new_sol, 0, sol, 0, new_sol.length); //sol=new_sol; parent copied as children for GA algorithm
      

    //compute fitness
    for(int j=0; j<POP_SIZE; j++) 
    {
    int count=0;
       for(int k=0; k<FEATURE_SIZE; k++) 
       {
       temp_sol[k]=sol[j][k];
         if (temp_sol[k])
         count++;
       }
    fitness[j]=(double)KNN(train, val, train_label, val_label, temp_sol) - (1.0*count/FEATURE_SIZE); 
    //System.out.print(fitness[j] + " ");
    }

    //write code to do selection
        double[] temp_elite = new double[5]; //array to store choosen fitness
        int[] rnd_array = new int[5]; //array of randomly choosen 5
        Random rn = new Random();

        for(int j=0; j<POP_SIZE; j++)
        {
            for(int i=0; i<5; i++) { //randomly select 5
                rnd_array[i] = rn.nextInt(POP_SIZE);
                temp_elite[i]=fitness[rnd_array[i]];
            }

            int maxAt = 0; //tournament selection
            for (int i = 0; i < temp_elite.length; i++)
                maxAt = temp_elite[i] > temp_elite[maxAt] ? i : maxAt;
            for (int k=0; k<FEATURE_SIZE; k++)
                new_sol[j][k] = sol[rnd_array[maxAt]][k];
        }

        //write code to do crossover
        boolean[] cross_array = new boolean[FEATURE_SIZE]; //array of temp crossover
        for(int j=0; j<POP_SIZE; j++)
        {
            if(rn.nextDouble()<0.5) //how many times, pcrossover=0.5
            {
                int rnda = rn.nextInt(POP_SIZE);  //random parent
                int rndb = rn.nextInt(POP_SIZE);  //random parent
                int rnd1 = rn.nextInt(FEATURE_SIZE);  //random crossover point
                int rnd2 = rn.nextInt(FEATURE_SIZE);  //random crossover point

                for (int i=rnd1; i<rnd2; i++)
                {
                    cross_array[i]=new_sol[rnda][i];
                    new_sol[rnda][i]=new_sol[rndb][i];
                    new_sol[rndb][i]=cross_array[i];
                }

            }
        }

        //write code to do mutation
        for(int j=0; j<POP_SIZE; j++) {
            if(rn.nextDouble()<0.1) { //how many times, pm=0.1
                int rnd = rn.nextInt(POP_SIZE);  //random parent
                int rnd1 = rn.nextInt(FEATURE_SIZE);  //random gene
                if (new_sol[rnd][rnd1])
                    new_sol[rnd][rnd1] = false;
                if (!new_sol[rnd][rnd1])
                    new_sol[rnd][rnd1] = true;
            }
        }

        int maxAt = 0;
    for (int j = 0; j < POP_SIZE; j++) 
    maxAt = fitness[j] > fitness[maxAt] ? j : maxAt;
       for (int k=0; k<FEATURE_SIZE; k++)
       final_sol[k]=sol[maxAt][k];
    System.out.println("Best fitness = " + fitness[maxAt]);

    //update the population, copy fitter children as parent for next generation
    //not that this line might need to removed (or kept) depending on how the GA operators are coded 

    
    } //end of gen loop

    int channel=0;
    for (int k=0; k<FEATURE_SIZE; k++)
       if (final_sol[k]){
       System.out.print(1 + " ");
       channel++;
       }
       else
       System.out.print(0 + " ");
    System.out.println("Channel count =  " + channel);
    
    System.out.println("Accuracy =  " + KNN(train, val, train_label, val_label, final_sol));

    
    //write best solution (features) to file, DO NOT MODIFY
     try{
        PrintWriter writer = new PrintWriter("kNN_GA_output.txt", "UTF-8");
          for(int j=0; j<FEATURE_SIZE; j++) 
            if (final_sol[j])
            writer.print("1 ");
            else
            writer.print("0 ");
        writer.close();
     }
     catch(Exception e)
     {
     System.out.println(e);
     }    
    
    return final_sol;

  }

  
  public static double KNN(double[][] train, double[][] val, int[] train_label, int[] val_label, boolean[] sol) {
    double[][] dist_label = new double[TRAIN_SIZE][2]; //distance array, no of columns+1 to accomodate distance
    double[] y = new double[FEATURE_SIZE];
    double[] x = new double[FEATURE_SIZE];

    int NUM_NEIGHBOUR = 10; //replace with optimal k value
    
    int[] neighbour = new int[NUM_NEIGHBOUR];
    int[] predicted_class = new int[VAL_SIZE];
          
    for (int j=0; j<VAL_SIZE; j++) //for every validation data
    {
       for (int f=0; f<FEATURE_SIZE; f++)
          if (sol[f])  
          y[f]=val[j][f]; 
          else
          y[f]=0.0; 
         
          for (int i=0; i<TRAIN_SIZE; i++) 
          {
             for (int f=0; f<FEATURE_SIZE; f++)
                if (sol[f])
                x[f]=train[i][f]; 
                else
                x[f]=0.0;

          double sum=0.0; //Manhattan distance
             for (int f=0; f<FEATURE_SIZE; f++)
             sum=sum + Math.abs(x[f]-y[f]);

          dist_label[i][0] = sum;
          dist_label[i][1] = train_label[i];
          }
   
         Sort(dist_label,1); //Sorting distance
    
         for (int n=0; n<NUM_NEIGHBOUR; n++) //training label from required neighbours
         neighbour[n]=(int) dist_label[n][1];
       
         predicted_class[j]=Mode(neighbour);
    
    } //end val data loop
       
   int success=0;
   for (int j=0; j<VAL_SIZE; j++)
      if (predicted_class[j]==val_label[j])
      success++;
   accuracy = (success*1.0)/VAL_SIZE;
   //System.out.print(accuracy + " ");    
     
   return accuracy;
   }

} //end class loop