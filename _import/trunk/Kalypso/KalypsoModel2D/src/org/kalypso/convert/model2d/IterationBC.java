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

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;

/**
 * -----------------------------------------------------------------
 * 
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 */
public class IterationBC {
    
    /**
     * Constructor
     */
    public IterationBC(){
        
    }
    
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
            String blockValues = iterationFE.getProperty("NITI")+"  "+iterationFE.getProperty("NITN")
								+"  "+iterationFE.getProperty("MBAND")+"  "+iterationFE.getProperty("NSTART")
								+"  "+iterationFE.getProperty("NCYC")+"  "+iterationFE.getProperty("DELT")
								+"  "+iterationFE.getProperty("LI")+"  "+iterationFE.getProperty("ITSI")
								+"  "+iterationFE.getProperty("ISPLPT")+"  "+iterationFE.getProperty("JSPLPT")
								+"  "+iterationFE.getProperty("IHOE")+"  "+iterationFE.getProperty("IDEN")
								+"  "+iterationFE.getProperty("ICONVG")+"  "+iterationFE.getProperty("IGRAV");
            
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
