/*
 * This class is for handling the <MeshPoints> belonging to one 
 *  <MeshPolygon>. Three or four <MeshPoints> are forming a <MeshPolygon>.
 * 
 * Created on 06.10.2004
 *
 */
package org.kalypso.convert.model2d;

/**
 * @author Katharina <a href="mailto:k.lupp@web.de>Katharina Lupp</a>
 *
 */
public class MeshPoint {
    
    private double x;
    private double y;
    private double z;

    
    private int id;
    
    /**
     * Constructor
     * @param id
     * @param float x
     * @param float y
     * @param float z
     */
    public MeshPoint(int id, double x, double y, double z){
        this.id = id;
        this.x = x;
        this.y = y;
        this.z = z;
    }

    
    
    /**
     * Constructor
     * @param id
     */
    public MeshPoint(int id){
        this.id = id;
    }
    
    /**
     * gets the x-coordinate of the MeshPoint
     * @return
     */
    public double getX(){
        return x;
    }
    
    /**
     * gets the y-coordinate of the MeshPoint
     * @return
     */
    public double getY(){
        return y;
    }
    
    /**
     * gets the z-coordinate of the MeshPoint
     * @return
     */
    public double getZ(){
        return z;
    }
    
    /**
     * gets the id of the MeshPoint
     * @return
     */
    public int getID(){
        return id;
    }
    
}
