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
 * -----------------------------------------------------------------
 * 
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 */
public class Viscosity {
    
    /**
     * Constructor
     */
    public Viscosity(){
        
    }
    
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
