/*
 * Created on 13.01.2005
 */
package org.kalypso.convert.model2d;

import java.util.ArrayList;

/**
 * -----------------------------------------------------------------
 * 
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */
public class Roughness {
    
    private String roughness;
    private boolean exists = false;
    private ArrayList roughnessList = new ArrayList();
    
    /**
     * 
     * Constructor
     */
    public Roughness(){
        
    }
    
    /**
     * gets the roughness parameters
     * @return
     */
    public ArrayList getRoughness(){
        return this.roughnessList;
    }
    
    /**
     * sets the roughness parameters encountered in an <ArrayList>
     * @param roughnessList
     */
    public void setRoughness(ArrayList roughnessList){
        this.roughnessList = roughnessList;
    }
    
    /**
     * adds roughness to vector and tests if it is already present
     * @param roughness
     */
    public void addRoughness(ArrayList list, String roughness, String color){
        roughness = roughness+"  "+color;
        exists = testRoughness(list, roughness);
        if(exists == false) {
            list.add(roughness);
            this.setRoughness(list);
        }
    }

    /**
     * tests if roughness is already present in vectorRoughness
     * @param roughness
     * @return
     */
    private boolean testRoughness(ArrayList list, String roughness){
        if(!list.isEmpty() && list.contains(roughness)) exists = true;
	    else exists = false;
        
        return exists;
    }
}
