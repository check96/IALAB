package main;

import aima.core.probability.Factor;
import aima.core.probability.RandomVariable;
import aima.core.probability.bayes.BayesianNetwork;
import aima.core.probability.bayes.FiniteNode;
import aima.core.probability.bayes.Node;
import aima.core.probability.proposition.AssignmentProposition;

import java.util.*;


public class Calculator {
    private Record record = new Record();
    private BayesianNetwork bayesNet;

    public Calculator(BayesianNetwork bayesNet) {
        this.bayesNet = bayesNet;
    }

    public void MPE(List<AssignmentProposition> evidences){

        System.out.println("Evidences = " + evidences);
        List<Factor> factors = new ArrayList<>();
        List<RandomVariable> evidenceVars = new ArrayList<>();
        List<RandomVariable> variables = new ArrayList<>();

        long start = System.currentTimeMillis();

        for (AssignmentProposition ap: evidences)
            evidenceVars.add(ap.getTermVariable());

        for (RandomVariable var : this.bayesNet.getVariablesInTopologicalOrder())
                variables.add(var);

        Collections.reverse(variables);
        for (RandomVariable var: variables){
            factors.add(makeFactor(var, evidences));
            if(!evidenceVars.contains(var))
                factors = maxOutFactors(var, factors);
        }

        Factor result = pointwiseProduct(factors);
        Map<RandomVariable, Object> assignments = retroPropagation(variables.size()-evidenceVars.size());
        long end = System.currentTimeMillis();

        System.out.println("\nP(MPE,e) = " + result);
        System.out.println("\n"+assignments);

        System.out.println("\nMPE calculated in " + (end-start) + " ms.\n");
    }

    public void MAP(List<RandomVariable> mapVars, List<AssignmentProposition> evidences) {

        System.out.println("Evidences = " + evidences);
        System.out.println("Map variables = " + mapVars);
        List<Factor> factors = new ArrayList<>();
        List<RandomVariable> evidenceVars = new ArrayList<>();
        List<RandomVariable> variables = new ArrayList<>();
        long start = System.currentTimeMillis();
        for (RandomVariable var : this.bayesNet.getVariablesInTopologicalOrder())
            variables.add(var);
        Collections.reverse(variables);

        for (AssignmentProposition ap: evidences)
            evidenceVars.add(ap.getTermVariable());

        for (RandomVariable var: variables) {
            factors.add(makeFactor(var,evidences));
            if (!evidenceVars.contains(var) && !mapVars.contains(var))
                factors = sumOutFactors(var, factors);
        }

        for (RandomVariable var: variables) {
            if(!evidenceVars.contains(var) && mapVars.contains(var))
                factors = maxOutFactors(var, factors);
        }

        Factor result = pointwiseProduct(factors);
        Map<RandomVariable, Object> assignments = retroPropagation(mapVars.size());
        long end = System.currentTimeMillis();

        System.out.println("\nP(MAP,e) = " + result);
        System.out.println("\n"+assignments);

        System.out.println("\nMAP calculated in " + (end-start) + " ms.\n");
    }

    private Map<RandomVariable,Object> retroPropagation(int numVars){

        Map<RandomVariable,Object> assignments = new HashMap<>();
        Map<RandomVariable, Object> assignment;
        while(assignments.size() < numVars && (assignment = record.getAssignmentFor(assignments)) != null)
            assignments.putAll(assignment);

        return assignments;
    }

    private Factor makeFactor(RandomVariable var, List<AssignmentProposition> evidences) {
        Node n = bayesNet.getNode(var);
        if (!(n instanceof FiniteNode))
            throw new IllegalArgumentException("makeFactor only works with finite Nodes.");

        FiniteNode fn = (FiniteNode) n;
        List<AssignmentProposition> evidence = new ArrayList<>();
        for (AssignmentProposition ap : evidences)
            if (fn.getCPT().contains(ap.getTermVariable()))
                evidence.add(ap);

        Factor factor = fn.getCPT().getFactorFor(evidence.toArray(new AssignmentProposition[evidence.size()]));

        return factor;
    }

    private List<Factor> maxOutFactors(RandomVariable var, List<Factor> factors) {
        List<Factor> maxedOut = new ArrayList<>();
        List<Factor> toMultiply = new ArrayList<>();
        for (Factor f : factors) {
            if (f.contains(var))
                toMultiply.add(f);
            else
                // This factor does not contain the variable
                maxedOut.add(f);
        }

        maxedOut.add(pointwiseProduct(toMultiply).maxOut(record,var));

        return maxedOut;
    }

    private List<Factor> sumOutFactors(RandomVariable var, List<Factor> factors) {
        List<Factor> summedOut = new ArrayList<>();

        List<Factor> toMultiply = new ArrayList<>();
        for (Factor f : factors) {
            if (f.contains(var))
                toMultiply.add(f);
            else
                // This factor does not contain the variable
                summedOut.add(f);
        }

        summedOut.add(pointwiseProduct(toMultiply).sumOut(var));

        return summedOut;
    }

    private Factor pointwiseProduct(List<Factor> factors) {

        Factor product = factors.get(0);
        for (int i = 1; i < factors.size(); i++)
            product = product.pointwiseProduct(factors.get(i));

        return product;
    }
    private void print(List<Factor> factors) {

        for (Factor f : factors) {
            System.out.println(f.getArgumentVariables());
            System.out.println(f);
        }

        System.out.println();
    }
}
