package main;

import aima.core.probability.RandomVariable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Record {

    Map<String,Map<RandomVariable,Object>> assignments = new HashMap<>();
    Map<String,Map<RandomVariable,Object>> assignmentsValues = new HashMap<>();

    public void add(Map<RandomVariable, Object> possibleWorld, RandomVariable var, Object obj) {
        String key = "";
        if(!possibleWorld.isEmpty())
            key = possibleWorld.toString();

        if(assignments.get(key) == null)
            assignments.put(key,new HashMap<>());
        else if(contains(assignments.get(key),var))
            assignments.get(key).remove(var);

        assignments.get(key).put(var,obj);

        Map<RandomVariable,Object> worlds = new HashMap<>();
        for (RandomVariable v : possibleWorld.keySet())
            worlds.put(v,possibleWorld.get(v));

        assignmentsValues.put(key,worlds);
    }

    public Map<RandomVariable,Object> getAssignmentFor(Map<RandomVariable,Object> previous) {

        List<String> remove = new ArrayList<>();
        String assignment = "a";

        for(String ass : assignments.keySet()){
            Map<RandomVariable, Object> combination = assignmentsValues.get(ass);
            if(!coerent(combination,previous))
                remove.add(ass);
            else if(isAssignment(combination,previous))
                    assignment = ass;
        }

        for (String ass : remove) {
            assignmentsValues.remove(ass);
            assignments.remove(ass);
        }


        if(!assignment.equals("a")) {
            assignmentsValues.remove(assignment);
            return assignments.remove(assignment);
        }

        return null;
    }

    private boolean contains(Map<RandomVariable, Object> list, RandomVariable var){
        for(RandomVariable v : list.keySet())
            if(v.equals(var))
                return true;

        return false;
    }

    private boolean isAssignment(Map<RandomVariable, Object> combination, Map<RandomVariable, Object> previous) {
        if(combination.size() > previous.size())
            return false;

        for (RandomVariable var: combination.keySet())
            if(!contains(previous,var))
                return false;

        return true;
    }

    private boolean coerent(Map<RandomVariable, Object> combination, Map<RandomVariable, Object> previous) {
        if(previous.isEmpty())
            return true;

        for (RandomVariable assignmentVar : previous.keySet()) {
            if(combination.containsKey(assignmentVar) &&
                    !combination.get(assignmentVar).equals(previous.get(assignmentVar)))
                return  false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "assignments: "+assignments + "\nvalues: " + assignmentsValues;
    }

}
