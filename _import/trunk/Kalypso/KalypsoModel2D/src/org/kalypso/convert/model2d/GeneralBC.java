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
 * ----------------------------------------------------------------------
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */
public class GeneralBC {
    
    /**
     * Constructor
     */
    public GeneralBC(){
        
    }
    
    /**
     * creates general properties of boundary conditions
     * @param ws
     * @param rootFeature
     */
    public StringBuffer createGeneral2BC(GMLWorkspace ws, Feature rootFeature){
        Feature generalCollectionFE = ws.resolveLink(rootFeature, "genCollectionMember");
        List generalList = (List) generalCollectionFE.getProperty("genMember");
        //TODO new addtional param group
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < generalList.size(); i++) {
            Feature generalFE = (Feature) generalList.get(i);  
            //TODO why is generalFE == null?
            if(generalFE != null){
	            String blockNames = "  FEM     MORPH       P_BOTTOM             MINEDDY";
	            String blockNames2 = "   AUSSTEU     ITURB           P_PRANDTL";
	            sb.append(blockNames+"\n");
	            sb.append(blockNames2+"\n");
	            
	            StringWriter stringWriter = new StringWriter();
	            PrintWriter printWriter = new PrintWriter(stringWriter);
	            String format="%5d%5d%5d%5d%10.3f%10.3f%10.1f";
	            Object[] o = new Object[]{
	                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("FEM")) ),
	                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("AUSSTEU")) ),
	                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("MORPH")) ),
	                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("ITURB")) ),
	                    new Double( Double.parseDouble(""+generalFE.getProperty("P_BOTTOM")) ),
	                    new Double( Double.parseDouble(""+generalFE.getProperty("P_PRANDTL")) ),
	                    new Double( Double.parseDouble(""+generalFE.getProperty("MINEDDY")) ),
	                    
	            };
	            printWriter.printf(Locale.ENGLISH,format+"\n", o);
//	            sb.append(blockNames+"\n");
//	            sb.append(blockNames2+"\n");

	            sb.append(stringWriter.getBuffer());
	            printWriter.close();
	            try {
	                stringWriter.close();
	            } catch (IOException e) {
	                e.printStackTrace();
	            }
	            sb.append("x"+"\n");
            }
        }
        
        return sb;
    }
    
    /**
     * 
     * @param ws
     * @param rootFeature
     * @return
     */
    public StringBuffer createGeneralBC(GMLWorkspace ws, Feature rootFeature){
        Feature generalCollectionFE = ws.resolveLink(rootFeature, "generalCollectionMember");
        List generalList = (List) generalCollectionFE.getProperty("generalMember");

        StringBuffer sb = new StringBuffer();
        for (Iterator iter = generalList.iterator(); iter.hasNext();) {
            Feature generalFE = (Feature) iter.next();                
            String blockNames = "   NE NMAT  NPX  NBX NWID NSID IPRT  NCL       IRO     IQGEN    ISTGEN    IDNOPT";
            String blockNames2 = "                                        IWIND     IRSLP     IHGEN     NCFLW     IBGEN";
            
            StringWriter stringWriter = new StringWriter();
            PrintWriter printWriter = new PrintWriter(stringWriter);
            String format="%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d";
            Object[] o = new Object[]{
                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("NE")) ),
                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("NMAT")) ),
                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("NPX")) ),
                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("NBX")) ),
                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("NWID")) ),
                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("NSID")) ),
                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("IPRT")) ),
                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("NCL")) ),
                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("IWIND")) ),
                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("IRO")) ),
                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("IRSLP")) ),
                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("IQGEN")) ),
                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("IHGEN")) ),
                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("ISTGEN")) ),
                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("NCFLW")) ),
                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("IDNOPT")) ),
                    new Integer( (int)Double.parseDouble(""+generalFE.getProperty("IBGEN")) ),
            };
            printWriter.printf(Locale.ENGLISH,format+"\n", o);
            sb.append(blockNames+"\n");
            sb.append(blockNames2+"\n");

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
