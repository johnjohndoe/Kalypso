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
