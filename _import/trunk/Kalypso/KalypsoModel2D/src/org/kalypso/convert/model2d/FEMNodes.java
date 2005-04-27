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
 * Created on 12.01.2005
 *
 */
package org.kalypso.convert.model2d;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import org.kalypso.java.io.PrintWriter;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * 
 * This class is for organizing the parameters of the <MeshPoint> for converting the
 * generated <GMLWorkspace> to the ascii file for the simulation programm.
 * 
 * The parameters generated are: FP and its subnodes parameters (va, ga ,vo, go and zu)
 * if they exist.
 * -----------------------------------------------------------------
 * 
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *
 */
public class FEMNodes {
    
    private StringBuffer sbNode = new StringBuffer();
    private int countNode = 1;
    private ArrayList nodeList = new ArrayList();
    
    private static float m_xoffset=3500000;
    private static float m_yoffset=5900000;
    
    private String va;
    private String zu;
    
    
    /**
     * creates node properties by using the <GMLWorkspace> and the rootFeature.
     * The properties are separated in the particular parameters defined in the
     * ascii file of the simulation programm.
     * 
     * @param ws
     * @param rootFeature
     */
    public void createNodeProperties(GMLWorkspace ws, Feature rootFeature){
        Feature fpCollectionFE = ws.resolveLink(rootFeature, "featurePointCollectionMember");
        List fpFEs = (List) fpCollectionFE.getProperty("featurePointMember");
        for (Iterator iter = fpFEs.iterator(); iter.hasNext();) {
            try{
                Feature fpFE = (Feature) iter.next();
                GM_Point point = (GM_Point) fpFE.getProperty("geometry");
                String x = ""+(point.getX()-m_xoffset);
                double xCoord = Double.parseDouble(x);
                String y = ""+(point.getY()-m_yoffset);
                double yCoord = Double.parseDouble(y);
                
                setFPFormatted(countNode, xCoord,yCoord,point.getZ());
                
                Node node = new Node(countNode, point.getX(), point.getY(), point.getZ());
                this.nodeList.add(node);
                
                if(fpFE.getProperty("xVelocity")!= null && fpFE.getProperty("yVelocity")!= null
                        && fpFE.getProperty("riverDepth")!=null && fpFE.getProperty("waterLevel")!= null){
                    va = "VA   "+countNode+"   "+fpFE.getProperty("xVelocity")
                    							 +"   "+fpFE.getProperty("yVelocity")
                    							 +"   "+fpFE.getProperty("riverDepth")
                    							 +"   "+fpFE.getProperty("waterLevel")+"\n";

                    if ( !(""+fpFE.getProperty("xVelocity")).equalsIgnoreCase("0") &&
                         !(""+fpFE.getProperty("yVelocity")).equalsIgnoreCase("0")) {
//                        System.out.println("000000000000000000000");
                        sbNode.append(va);

                        this.addFPInfo(fpFE, "GA","timeGradient1", "timeGradient2","timeGradient3", sbNode, countNode);
                        this.addFPInfo(fpFE, "VO","degreeOfFreedomOfOldTimeGradient1", "degreeOfFreedomOfOldTimeGradient2",
                                		"degreeOfFreedomOfOldTimeGradient3", sbNode, countNode);
                        this.addFPInfo(fpFE, "GO","TimeGradientOfFormerTimeWarps1", "TimeGradientOfFormerTimeWarps2",
                                		"TimeGradientOfFormerTimeWarps3", sbNode, countNode);
                    }
                }

                countNode++;
                
            }catch(Exception ex){
                    System.out.println("no node feature could be generated ");
                    ex.printStackTrace();
            }
        }
        
        this.setSB_FP(sbNode);
    }
    
    private void setFPFormatted(int id, double x, double y, double z){
        StringWriter stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(stringWriter);
        String format="FP%10d%20.7f%20.7f%20.7f";
        Object[] o = new Object[]{
                new Integer(id),
                new Double(x),
                new Double(y),
                new Double(z),
        };
        printWriter.printf(Locale.ENGLISH,format+"\n", o);

        this.sbNode.append(stringWriter.getBuffer());
        printWriter.close();
        try {
            stringWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    
    /**
     * sets additional information of a feature point generated by the simulation
     * 
     * @param fpFE
     * @param type
     * @param info1
     * @param info2
     * @param info3
     * @param sbNode
     * @param countNode
     */
    private void addFPInfo(Feature fpFE, String type, String info1, String info2, String info3, StringBuffer sbNode, int countNode){
        if(fpFE.getProperty(info1)!= null && fpFE.getProperty(info2)!= null 
                && fpFE.getProperty(info3)!= null){
            String s = type+"   "+countNode+"   "+ fpFE.getProperty(info1)+"   "
            			+ fpFE.getProperty(info2)+"   "+ fpFE.getProperty(info3)+"\n";
            zu = "ZU   "+countNode+"   0   0   0   0 "+"\n";
            sbNode.append(s);
            sbNode.append(zu);
        }
    }
    
    /**
     * gets the <StringBuffer> of the nodes
     * @param sbNode
     */
    public void setSB_FP(StringBuffer sbNode){
        this.sbNode = sbNode;
    }
    
    /**
     * sets the <StringBuffer> of the nodes
     * @return sbNode
     */
    public StringBuffer getSB_FP(){
        return this.sbNode;
    }
    
    /**
     * gets the <ArrayList> of the <Nodes>
     */
    public ArrayList getNodeList(){
        return this.nodeList;
    }

    
   
}
