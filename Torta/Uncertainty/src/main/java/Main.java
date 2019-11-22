import aima.core.probability.Factor;
import aima.core.probability.RandomVariable;
import aima.core.probability.bayes.ConditionalProbabilityDistribution;
import aima.core.probability.bayes.FiniteNode;
import aima.core.probability.bayes.Node;
import aima.core.probability.bayes.BayesianNetwork;
import aima.core.probability.bayes.impl.BayesNet;
import aima.core.probability.bayes.impl.FullCPTNode;
import aima.core.probability.domain.BooleanDomain;
import aima.core.probability.proposition.AssignmentProposition;
import aima.core.probability.util.RandVar;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class Main {

    BayesianNetwork bayesNet;
    List<RandomVariable> mapVariables = new ArrayList<RandomVariable>();
    List<RandomVariable> noMapVariables = new ArrayList<RandomVariable>();
    boolean[] array = new boolean[] {true,false};
    HashMap<String,Factor> hash;

    public Main() { }

    public BayesianNetwork parserBayesNet(){
        BifBNReader parser = null;
        try {
            parser = new BifBNReader("sources\\earthquake.bif") {
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

    public void algorithm_MAP(){
        bayesNet = parserBayesNet();

        for (RandomVariable variable : bayesNet.getVariablesInTopologicalOrder())
            if(variable.getName().equalsIgnoreCase("A"))
                noMapVariables.add(variable);
            else
                mapVariables.add(variable);

        HashMap<String, Factor> hash = getHash();

        Factor factor = hash.get("MA").pointwiseProduct(hash.get("ABE")).pointwiseProduct(hash.get("JA"));
        Factor factor1 = factor.sumOut(noMapVariables.get(0));

        for (double d : factor1.getValues()) {
            System.out.println(d);
        }
    }

    public HashMap<String,Factor> getHash(){
        HashMap<String,Factor> hash = new HashMap<String, Factor>();

        for (RandomVariable var : noMapVariables) {
            FiniteNode node = (FiniteNode)bayesNet.getNode(var);
            String key = var.getName();
            for (Node n : node.getParents())
                key += n.getRandomVariable().getName();
            hash.put(key,node.getCPT().getFactorFor(new AssignmentProposition[]{}));

            for (RandomVariable mapVar : mapVariables) {
                FiniteNode mapNode = (FiniteNode)bayesNet.getNode(mapVar);
                if(mapNode.getParents().contains(node)) {
                    key = mapVar.getName();
                    for (Node n : mapNode.getParents())
                        key += n.getRandomVariable().getName();
                    hash.put(key,mapNode.getCPT().getFactorFor(new AssignmentProposition[]{}));
                }
            }
        }
        System.out.println(hash);
        return hash;
    }
    public static void main(String[] args) {
        try {
            new Main().algorithm_MAP();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
