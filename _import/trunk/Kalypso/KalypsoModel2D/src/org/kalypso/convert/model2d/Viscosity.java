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
import java.io.StringWriter;
import java.util.List;
import java.util.Locale;

import org.kalypso.java.io.PrintWriter;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * -----------------------------------------------------------------
 * 
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 */
public class Viscosity {
    
    /**
     * creates the viscosity of boundary conditions
     * @param ws
     * @param rootFeature
     */
    public StringBuffer createViscosity(GMLWorkspace ws, Feature rootFeature){
        Feature viscosityCollectionFE = ws.resolveLink(rootFeature, "viscosityCollectionMember");
        List list = (List) viscosityCollectionFE.getProperty("viscosityMember");
        
        StringBuffer sb = new StringBuffer();
        sb.append("NMAT Zeilen Zuordnung von Rauhigkeitsklasse"+"\n");
        sb.append("  zu                          4*Wirbel-Viscositaet  -Kst/+Ks      abst durchbaum   d50[mm]"+"\n");
        
        Object[] o;
        for (int i = 0; i < list.size(); i++) {
            Feature viscosityFE = (Feature) list.get(i);
            StringWriter stringWriter = new StringWriter();
            PrintWriter printWriter = new PrintWriter(stringWriter);
            String format = "%10d%10.2f%10.2f%10.2f%10.2f%11.2f";
            o = new Object[]{
                    new Integer( (int)Double.parseDouble(""+viscosityFE.getProperty("id")) ),
                    new Double( (int)Double.parseDouble(""+viscosityFE.getProperty("wv1")) ),
                    new Double( (int)Double.parseDouble(""+viscosityFE.getProperty("wv2")) ),
                    new Double( (int)Double.parseDouble(""+viscosityFE.getProperty("wv3")) ),
                    new Double( (int)Double.parseDouble(""+viscosityFE.getProperty("wv4")) ),
                    new Double( Double.parseDouble(""+viscosityFE.getProperty("ks")) ), 
            };

            if(viscosityFE.getProperty("abst")!= null){
                List l = (List)viscosityFE.getProperty("abst");
                if(l.size()>0){
	                format = "%10d%10.2f%10.2f%10.2f%10.2f%11.2f%9.1f";
                    o = new Object[]{
                            new Integer( (int)Double.parseDouble(""+viscosityFE.getProperty("id")) ),
                            new Double( (int)Double.parseDouble(""+viscosityFE.getProperty("wv1")) ),
                            new Double( (int)Double.parseDouble(""+viscosityFE.getProperty("wv2")) ),
                            new Double( (int)Double.parseDouble(""+viscosityFE.getProperty("wv3")) ),
                            new Double( (int)Double.parseDouble(""+viscosityFE.getProperty("wv4")) ),
                            new Double( Double.parseDouble(""+viscosityFE.getProperty("ks")) ), 
                            new Double( Double.parseDouble(""+l.get(0)) ), 
                    };
                }
            }
            if(viscosityFE.getProperty("durchbaum")!= null){
                List l = (List)viscosityFE.getProperty("durchbaum");
                List lAbst = (List)viscosityFE.getProperty("abst");
                if(l.size()>0){
                    format="%10d%10.2f%10.2f%10.2f%10.2f%11.2f%9.1f%10.2f";
                    o = new Object[]{
                            new Integer( (int)Double.parseDouble(""+viscosityFE.getProperty("id")) ),
                            new Double( (int)Double.parseDouble(""+viscosityFE.getProperty("wv1")) ),
                            new Double( (int)Double.parseDouble(""+viscosityFE.getProperty("wv2")) ),
                            new Double( (int)Double.parseDouble(""+viscosityFE.getProperty("wv3")) ),
                            new Double( (int)Double.parseDouble(""+viscosityFE.getProperty("wv4")) ),
                            new Double( Double.parseDouble(""+viscosityFE.getProperty("ks")) ), 
                            new Double( Double.parseDouble(""+lAbst.get(0)) ), 
                            new Double( Double.parseDouble(""+l.get(0)) ), 
                    };
                }
            }
            printWriter.printf(Locale.ENGLISH,format+"\n", o);
            
            sb.append(stringWriter.getBuffer());
            printWriter.close();
            try {
                stringWriter.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        sb.append("x"+"\n");
        
        return sb;
    }
    
    

}
