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
 * Created on 20.01.2005
 */
package org.kalypso.convert.model2d;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * -----------------------------------------------------------------
 * 
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 */
public class IterationBC {
    
    /**
     * creates iteration properties of boundary conditions
     * @param ws
     * @param rootFeature
     */
    public StringBuffer createIterationProp(GMLWorkspace ws, Feature rootFeature){
        Feature iterationCollectionFE = ws.resolveLink(rootFeature, "iterationCollectionMember");
        List list = (List) iterationCollectionFE.getProperty("iterationMember");
        StringBuffer sb = new StringBuffer();
        for (Iterator iter = list.iterator(); iter.hasNext();) {
            Feature iterationFE = (Feature) iter.next();       
//            String blockNames = "NITI  NITN  MBAND   NSTART   NCYC   DELT  LI   ITSI  ISPLPT  " +
//            					"JSPLPT   IHOE   IDEN   ICONVG   IGRAV";
            String blockNames = " NITI     MBAND      NCYC      DELT   LI ITSI    JSPLPT IHOE IDEN     IGRAV";
            String blockNames2 = "      NITN    NSTART                        ISPLPT              ICONVG   IPASCHE";
            
            sb.append(blockNames+"\n");
            sb.append(blockNames2+"\n");
            
            StringWriter stringWriter = new StringWriter();
            PrintWriter printWriter = new PrintWriter(stringWriter);
            String format="%5d%5d%5d%5d%5d%10.6f%5d%5d%5d%5d%5d%5d%5d%5d%5d";
            Object[] o = new Object[]{
                    new Integer( (int)Double.parseDouble(""+iterationFE.getProperty("NITI")) ),
                    new Integer( (int)Double.parseDouble(""+iterationFE.getProperty("NITN")) ),
                    new Integer( (int)Double.parseDouble(""+iterationFE.getProperty("MBAND")) ),
                    new Integer( (int)Double.parseDouble(""+iterationFE.getProperty("NSTART")) ),
                    new Integer( (int)Double.parseDouble(""+iterationFE.getProperty("NCYC")) ),
                    new Double( Double.parseDouble(""+iterationFE.getProperty("DELT")) ),
                    new Integer( (int)Double.parseDouble(""+iterationFE.getProperty("LI")) ),
                    new Integer( (int)Double.parseDouble(""+iterationFE.getProperty("ITSI")) ),
                    new Integer( (int)Double.parseDouble(""+iterationFE.getProperty("ISPLPT")) ),
                    new Integer( (int)Double.parseDouble(""+iterationFE.getProperty("JSPLPT")) ),
                    new Integer( (int)Double.parseDouble(""+iterationFE.getProperty("IHOE")) ),
                    new Integer( (int)Double.parseDouble(""+iterationFE.getProperty("IDEN")) ),
                    //TODO iConf in schema integrieren
//                    new Integer( (int)Double.parseDouble(""+iterationFE.getProperty("ICONVG")) ),
                    new Integer( 0 ),
                    new Integer( (int)Double.parseDouble(""+iterationFE.getProperty("IGRAV")) ),
                    //TODO IPASCHE
                    new Integer(0),
                    
            };
            printWriter.printf(Locale.ENGLISH,format+"\n", o);

            sb.append(stringWriter.getBuffer());
            printWriter.close();
            try {
                stringWriter.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
            
//            sb.append(blockValues+"\n");
            sb.append("x"+"\n");
        }
        
        return sb;
    }

}
