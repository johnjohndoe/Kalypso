/*
 * Created on 13.01.2005
 *
 */
package org.kalypso.convert.model2d;


/**
 * This class defines the node with id, x, y and z value.
 * -----------------------------------------------------------------
 * 
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */
public class Node {
    
    private int id;
    private double x;
    private double y;
    private double z;
    
    /**
     * Constructor
     * @param id
     * @param x
     * @param y
     * @param z
     */
    public Node(int id, double x, double y, double z){
        this.id = id;
        this.x = x;
        this.y = y;
        this.z = z;
    }
    
    /**
     * gets the id of <Node>
     * @return
     */
    public int getID(){
        return this.id;
    }
    
    /**
     * sets the id of <Node>
     * @param id
     */
    public void setID(int id){
        this.id = id;
    }
    
    /**
     * gets x of <Node>
     * @return
     */
    public double getX(){
        return this.x;
    }
    
    /**
     * sets x of <Node>
     * @param x
     */
    public void setX (double x){
        this.x = x;
    }
    
    /**
     * gets y of <Node>
     * @return
     */
    public double getY(){
        return this.y;
    }
    
    /**
     * sets y of <Node>
     * @param y
     */
    public void setY (double y){
        this.y = y;
    }
    
    /**
     * gets z of <Node>
     * @return
     */
    public double getZ(){
        return this.z;
    }
    
    /**
     * sets z of <Node>
     * @param z
     */
    public void setZ (double z){
        this.z = z;
    }
    


}
