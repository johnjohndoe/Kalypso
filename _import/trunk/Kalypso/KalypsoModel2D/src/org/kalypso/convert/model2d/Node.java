/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of ekalypso:
 Internet based elearning for complex simulation applications
 ("Internet basiertes E-Learning an komplexen Simulationsprogrammen [de]")

 The implementation is realised by: 
 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 The project is sponsored and supported by:  
 local authority of education and research, 
 E-Learning Consortium Hamburg (ELCH) and
 Multimedia Kontor Hamburg.
 
 As this implementation depends on third party open source 
 java code it is consequently also licenced as open source
 in the hope that it will be useful specially (but not exclusively)
 to other e-learning projects that may extend or use this project.
 
 Copyright (C) 2004, 2005 by:
 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 katharina.lupp@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
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
