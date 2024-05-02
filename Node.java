import java.util.*;

public class Node {
    private int id;
    private ArrayList<Integer> listOfLink;

    public Node(int id){
        this.id=id;
    }

    public void addLink(int otherNodeId){
        this.listOfLink.add(otherNodeId);
    }

}
