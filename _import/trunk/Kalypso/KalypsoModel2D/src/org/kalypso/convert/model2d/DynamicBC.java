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
import java.util.List;
import java.util.Locale;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;

/**
 * ----------------------------------------------------------------------
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 */
public class DynamicBC {
    
    /**
     * Constructor
     */
    public DynamicBC(){
        
    }
    
    /**
     * creates the dynamic boundary conditions
     * @param ws
     * @param rootFeature
     */
    public StringBuffer createDynamicBC(GMLWorkspace ws, Feature rootFeature){
        Feature dynamicCollectionFE = ws.resolveLink(rootFeature, "dynamicBC_CollectionMember");
        List list = (List) dynamicCollectionFE.getProperty("dynamicBC_Member");
        StringBuffer sb = new StringBuffer();
        StringBuffer qfhf = new StringBuffer();
        String names = "";
        StringWriter stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(stringWriter);
        String format = "%10d%10d%10.5f%10d%10d%10d%10d%10d";
        Object[] o;

        for (int i = 0; i < list.size(); i++) {
            Feature dynamicFE = (Feature) list.get(i);   
            names = "       nbx      nsid      delt     iqgen     ihgen    istgen     ncflw idenIwindIbgen";
            sb.append("DYNAMISCHE Randbedingungen fuer den   "+(i+1)+"-ten der NCYC Zeitschritte"+"\n");
	        sb.append(names+"\n");
	        
	        if (dynamicFE.getProperty("idenIwindIbgen")!=null){
		        o = new Object[]{
		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("nbx")) ),
		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("nsid")) ),
		                new Double( Double.parseDouble(""+dynamicFE.getProperty("delt")) ),
		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("iqgen")) ),
		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("ihgen")) ),
		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("istgen")) ),
		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("ncflw")) ),
		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("idenIwindIbgen")) ),
		        };
		        printWriter.printf(Locale.ENGLISH,format+"\n", o);
		        
	        }else{
	            format = "%10d%10d%10.5f%10d%10d%10d";
	            o = new Object[]{
		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("nbx")) ),
		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("nsid")) ),
		                new Double( Double.parseDouble(""+dynamicFE.getProperty("delt")) ),
		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("iqgen")) ),
		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("ihgen")) ),
		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("istgen")) ),
		        };
		        printWriter.printf(Locale.ENGLISH,format+"\n", o);
	        }
//	        }else{
//	            format = "%10d%10d%10.5f%10d%10d%10d%10d";
//	            o = new Object[]{
//		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("nbx")) ),
//		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("nsid")) ),
//		                new Double( Double.parseDouble(""+dynamicFE.getProperty("delt")) ),
//		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("iqgen")) ),
//		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("ihgen")) ),
//		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("istgen")) ),
//		                new Integer( (int)Double.parseDouble(""+dynamicFE.getProperty("ncflw")) ),
//		        };
//		        printWriter.printf(Locale.ENGLISH,format+"\n", o);
//	        }
	        sb.append(stringWriter.getBuffer());
	        sb.append("           IQGEN Zeilen: an NCL-Linie j Durchfluss qf in Richtung qdir vorgegeben."+"\n");
	        sb.append("         (qdir=0.0 Geschwindigkeit senkrecht zum jeweiligen Liniensegment)"+"\n");
	        
            Feature fe = (Feature)dynamicFE.getProperty("nclType");           
            List list2 = (List) fe.getProperty("dischargeMember"); 
            DischargeBC discharge = new DischargeBC();
            qfhf = discharge.getQFHFDyn(list2);
	        sb.append(qfhf);
	        
	        printWriter.close();
            try {
                stringWriter.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
            
        }
        
        return sb;
    }

}
