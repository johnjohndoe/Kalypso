/*
 * Created on 21.10.2004
 * 
 * This class is for getting the ids of the edges out of a mesh polygon.
 *
 */
package org.kalypso.convert.model2d;

import java.util.ArrayList;
import java.util.Vector;

/**
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 */
public class Edge {

    private ArrayList edgesList = new ArrayList();

    private boolean present = false;

    private Vector edgeOfRectangle = new Vector();

    private int id = 0;

    private int p1 = 0;

    private int p2 = 0;

    private int size = 0;

    private int idFE1 = 0;

    private int idFE2 = 0;

    /**
     * Constructor
     */
    public Edge() {

    }

    /**
     * Constructor
     * 
     * @param id
     */
    public Edge(int id) {
        this.id = id;
    }

    /**
     * Constructor
     * 
     * @param id
     * @param p1
     * @param p2
     */
    public Edge(int id, int p1, int p2) {
        this.id = id;
        this.p1 = p1;
        this.p2 = p2;
    }

    /**
     * Constructor
     * 
     * @param id
     * @param p1
     * @param p2
     * @param idFE1
     * @param idFE2
     */
    public Edge(int id, int p1, int p2, int idFE1, int idFE2) {
        this.id = id;
        this.p1 = p1;
        this.p2 = p2;
        this.idFE1 = idFE1;
        this.idFE2 = idFE2;
    }

    /**
     * returns a boolean with the information if this <Edge>is already present
     * in the <HashMap>of <Edges> and if it is present the <Edge> is clockwise.
     */
    public boolean existEdgeClockwise(ArrayList mapEdges, int idFE) {
        boolean exists = false;
        for (int i = 0; i < mapEdges.size(); i++) {
            Edge ed = (Edge) mapEdges.get(i);
            if (ed != null) {
                if (this.p1 == ed.getP1() && this.p2 == ed.getP2()){
                    exists = true; 
                    break;
                }
               
            }
        }
        return exists;
    }

    /**
     * returns a boolean with the information if this <Edge>is already present
     * in the <HashMap>of <Edges> and if it is present the <Edge> is not clockwise.
     * @param mapEdges
     * @param idFE
     * @return
     */
    public boolean existEdgeNotClockwise(ArrayList mapEdges, int idFE) {
        boolean exists = false;
        for (int i = 0; i < mapEdges.size(); i++) {
            Edge ed = (Edge) mapEdges.get(i);
            if (ed != null) {
               if(this.p2 == ed.getP1() && this.p1 == ed.getP2()){
                    exists = true; 
                    break;
                }
            }
        }
        return exists;
    }

    /**
     * 
     * @param idFE
     * @param mapEdges
     */
    public void setFElements(int idFE, ArrayList mapEdges) {

    }

    /**
     * gets p1 of <Edge>
     * 
     * @return
     */
    public int getP1() {
        return this.p1;
    }

    /**
     * sets p1 of <Edge>
     * 
     * @param point2
     */
    public void setP1(int point2) {
        this.p2 = point2;
    }

    /**
     * gets p2 of <Edge>
     * 
     * @return
     */
    public int getP2() {
        return this.p2;
    }

    /**
     * sets p2 of <Edge>
     * 
     * @param point2
     */
    public void setP2(int point2) {
        this.p2 = point2;
    }

    /**
     * gets id of <Edge>
     * 
     * @return
     */
    public int getID() {
        return this.id;
    }

    public void setID(int id){
        this.id = id;
    }
    /**
     * 
     * @return first element of <Edge>
     */
    public int getFE1() {
        return this.idFE1;
    }

    /**
     * sets first element of <Edge>
     * 
     * @param idFE1
     */
    public void setFE1(int idFE1) {
        this.idFE1 = idFE1;
    }

    /**
     * 
     * @return second element of <Edge>
     */
    public int getFE2() {
        return this.idFE2;
    }

    /**
     * sets second element of <Edge>
     * 
     * @param idFE1
     */
    public void setFE2(int idFE2) {
        this.idFE2 = idFE2;
    }

}