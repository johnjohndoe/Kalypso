/*
 * Created on 20.01.200
 */
package org.kalypso.convert.model2d;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;

/**
  * -----------------------------------------------------------------
 * 
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */
public class BCGeom {

    /**
     * Constructor
     */
    public BCGeom(){
        
    }
    
    /**
     * creates the second group of parameters for boundary conditions
     * @param ws
     * @param rootFeature
     */
    public StringBuffer createBCGeom(GMLWorkspace ws, Feature rootFeature){
        Feature geomCollectionFE = ws.resolveLink(rootFeature, "geomCollectionMember");
        List geomList = (List) geomCollectionFE.getProperty("geomMember");
        StringBuffer sb = new StringBuffer();
        for (Iterator iter = geomList.iterator(); iter.hasNext();) {
            Feature geomFE = (Feature) iter.next();                
            String blockNames = "     OMEGA     ,ELEV,   XSCALE,   ZSCALE,     DSET    ,DSETD,     UNOM,     HMIN   ,ASCALE      URFC";
            sb.append(blockNames+"\n");
            
            StringWriter stringWriter = new StringWriter();
            PrintWriter printWriter = new PrintWriter(stringWriter);
            String format="%10.2f%10.3f%10.2f%10.2f%10.2f%10.2f%10.2f%10.2f%10.2f%10.2f";
            Object[] o = new Object[]{
                    new Double( Double.parseDouble(""+geomFE.getProperty("OMEGA")) ),
                    new Double( Double.parseDouble(""+geomFE.getProperty("ELEV")) ),
                    new Double( Double.parseDouble(""+geomFE.getProperty("XSCALE")) ),
                    new Double( Double.parseDouble(""+geomFE.getProperty("ZSCALE")) ),
                    new Double( Double.parseDouble(""+geomFE.getProperty("DSET")) ),
                    new Double( Double.parseDouble(""+geomFE.getProperty("DSETD")) ),
                    new Double( Double.parseDouble(""+geomFE.getProperty("UNOM")) ),
                    new Double( Double.parseDouble(""+geomFE.getProperty("HMIN")) ),
                    new Double( Double.parseDouble(""+geomFE.getProperty("ASCALE")) ),
                    new Double( Double.parseDouble(""+geomFE.getProperty("URFC")) ),
                    
            };
            printWriter.printf(Locale.ENGLISH,format+"\n", o);

            sb.append(stringWriter.getBuffer());
            printWriter.close();
            try {
                stringWriter.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
            
            sb.append("x"+"\n");
        }
        
        return sb;
    }
    
    
}
