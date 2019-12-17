package main;

import aima.core.probability.RandomVariable;
import aima.core.probability.bayes.BayesianNetwork;
import aima.core.probability.bayes.FiniteNode;
import aima.core.probability.bayes.Node;
import aima.core.probability.bayes.impl.FullCPTNode;
import aima.core.probability.domain.FiniteDomain;
import aima.core.probability.proposition.AssignmentProposition;
import aima.core.search.csp.Assignment;
import parser.BifBNReader;

import java.io.File;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.Set;

public class Main {
    private BayesianNetwork bayesNet;
    private Scanner scanner = new Scanner(System.in);
    private boolean test = true;
    public void start(){

        bayesNet = chooseNetwork();
        Calculator calculator = new Calculator(bayesNet);
        List<AssignmentProposition> evidences = new ArrayList<>();

        while(true) {

            System.out.println("0) Change network\n" +
                    "1) Set evidences\n" +
                    "2) Calculate MAP\n" +
                    "3) Calculate MPE\n" +
                    /*"4) test map vars\n" +
                    "5) test evidences map\n" +
                    "6) test evidences mpe\n" +*/
                    "\n" +
                    "Choose the index: ");

            String index = scanner.next();

            switch (index) {
                case "0":
                    bayesNet = chooseNetwork();
                    calculator = new Calculator(bayesNet);
                    break;
                case "1":
                    evidences = setEvidences();
                    break;
                case "2":
                    MAP(calculator, evidences);
                    break;
                case "3": calculator.MPE(evidences);
                    break;
                case "4":
                    testMAP(calculator);
                    break;
                case "5":
                    testEvidencesMap(calculator);
                    break;
                case "6":
                    testEvidencesMpe(calculator);
                    break;
            }

            if(index.equals("2") || index.equals("3")) {
                System.out.println("Press 0 to terminate or any other key to continue");
                index = scanner.next();
                if (index.equals("0"))
                    break;
                evidences = new ArrayList<>();
            }
        }
        scanner.close();
    }

    private void testEvidencesMpe(Calculator calculator) {
        List<RandomVariable> variables = bayesNet.getVariablesInTopologicalOrder();

        for (int numVars = 1; numVars <= variables.size(); numVars ++) {
            List<RandomVariable> vars = new ArrayList<>();
            for (RandomVariable var: variables)
                vars.add(var);

            List<AssignmentProposition> evidences = new ArrayList<>();
            while(evidences.size() < numVars) {
                int i = (int) (Math.random() * vars.size());
                RandomVariable var = vars.remove(i);
                FiniteDomain domain = ((FiniteDomain)var.getDomain());
                int j = (int) (Math.random() * domain.size());
                Object obj = ((FiniteDomain)var.getDomain()).getValueAt(j);

                evidences.add(new AssignmentProposition(var,obj));
            }

            System.out.println("-------------- NUM EVIDENCES: " + evidences.size() + " ---------------");
            calculator.MPE(evidences);
        }
    }

    private void testEvidencesMap(Calculator calculator) {
        List<RandomVariable> variables = bayesNet.getVariablesInTopologicalOrder();


        for (int numVars = 1; numVars <= variables.size(); numVars ++) {
            List<RandomVariable> vars = new ArrayList<>();
            for (RandomVariable var: variables)
                vars.add(var);

            List<AssignmentProposition> evidences = new ArrayList<>();
            while(evidences.size() < numVars) {
                int i = (int) (Math.random() * vars.size());
                RandomVariable var = vars.remove(i);
                FiniteDomain domain = ((FiniteDomain)var.getDomain());
                int j = (int) (Math.random() * domain.size());
                Object obj = ((FiniteDomain)var.getDomain()).getValueAt(j);

                evidences.add(new AssignmentProposition(var,obj));
            }

            List<RandomVariable> mapVars = new ArrayList<>();
            int size = vars.size()/2;
            while(mapVars.size() < size) {
                int i = (int) (Math.random() * vars.size());
                mapVars.add(vars.remove(i));
            }


            System.out.println("-------------- NUM EVIDENCES: " + evidences.size() + " ---------------");
            calculator.MAP(mapVars,evidences);
        }

    }


    private List<AssignmentProposition> setEvidences() {

        List<AssignmentProposition> evidences = new ArrayList<>();
        List<RandomVariable> variables = bayesNet.getVariablesInTopologicalOrder();

        while(true){
            System.out.println("These are the available variables:\n" +
                    "0) back to main menù");

            for(int i = 0; i<variables.size(); i++)
                System.out.println((i+1) +")  " + variables.get(i).getName());

            System.out.println("Choose the variable index: ");
            int index = scanner.nextInt();
            if(index == 0)
                break;

            RandomVariable var = variables.get(index-1);
            FiniteDomain domain = (FiniteDomain) var.getDomain();

            System.out.println("What value do you want to assign to " + var.getName() + "?");
            Set<?> possibleValues = domain.getPossibleValues();
            int i = 0;
            List<Object> values = new ArrayList<>();
            for (Object obj: possibleValues) {
                System.out.println(i+") " + obj);
                i++;
                values.add(obj);
            }
            System.out.println("Choose the value index: ");
            index = scanner.nextInt();
            evidences.add(new AssignmentProposition(var,values.get(index)));
        }

        return evidences;
    }

    private void testMAP(Calculator calculator) {

        List<RandomVariable> variables = bayesNet.getVariablesInTopologicalOrder();

        for (int numVars = 25; numVars <= variables.size(); numVars++) {
            List<RandomVariable> vars = new ArrayList<>();
            for (RandomVariable var: variables)
                vars.add(var);
            List<RandomVariable> mapVars = new ArrayList<>();
            while(mapVars.size() < numVars) {
                int i = (int) (Math.random() * vars.size());
                mapVars.add(vars.remove(i));
            }

            System.out.println("-------------- NUM VARS: " + mapVars.size() + " ---------------");
            calculator.MAP(mapVars, new ArrayList<>());
        }
    }

    private void MAP(Calculator calculator, List<AssignmentProposition> evidences) {
        List<RandomVariable> mapVars = new ArrayList<>();
        List<RandomVariable> variables = bayesNet.getVariablesInTopologicalOrder();
        List<RandomVariable> evidenceVars = new ArrayList<>();
        for (AssignmentProposition ap: evidences)
            evidenceVars.add(ap.getTermVariable());

        while(true) {
            System.out.println("Choose map variables: \n" +
                    "0) calculate MAP");

            for (int i = 0; i < variables.size(); i++) {
                RandomVariable var = variables.get(i);
                if (!evidenceVars.contains(var)) {
                    if (!mapVars.contains(variables.get(i)))
                        System.out.println((i + 1) + ")  " + var);
                    else
                        System.out.println(var + " has already been selected");
                }
                else
                    System.out.println(var + " is an evidence");
            }

            int index = scanner.nextInt();
            if(index == 0)
                break;

            mapVars.add(variables.get(index-1));
        }

        calculator.MAP(mapVars,evidences);
    }

    public BayesianNetwork chooseNetwork() {

        File directory = new File("sources");
        File[] paths = directory.listFiles();
        System.out.println("These are the available networks");

        for (int i = 0; i < paths.length; i++)
            System.out.println(i + ")  " + paths[i].getName());

        System.out.println("Choose the network index: ");

        int index = 0;
        try {
            index = Integer.parseInt(scanner.next());
        }catch (Exception e) {
            return chooseNetwork();
        }
        return parserBayesNet(""+paths[index]);
    }

    private BayesianNetwork parserBayesNet(String path){
        BifBNReader parser = null;
        try {
            parser = new BifBNReader(path) {
                @Override
                protected Node nodeCreation(RandomVariable variable, double[] probs, Node... parents) {
                    return new FullCPTNode(variable,probs,parents);
                }
            };
        } catch (Exception e) {
            e.printStackTrace();
        }

        return parser.getBayesianNetwork();
    }

    public static void main(String[] args) {
        new Main().start();
    }
}
