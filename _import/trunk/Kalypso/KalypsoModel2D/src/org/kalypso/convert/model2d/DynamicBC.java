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
