/*
 * Created on 20.01.2005
 *
 */
package org.kalypso.convert.model2d;

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
public class DischargeBC {

    /**
     * Constructor
     */
    public DischargeBC(){
        
    }
    
    /**
     * creates the discharge block of boundary conditions
     * @param ws
     * @param rootFeature
     */
    public StringBuffer createDischarge(GMLWorkspace ws, Feature rootFeature){
        Feature dischargeCollectionFE = ws.resolveLink(rootFeature, "dischargeCollectionMember");
        List list = (List) dischargeCollectionFE.getProperty("dischargeMember"); 
        StringBuffer sb = getQFHF(list);      
                
        return sb;
    }
    
    /**
     * gets water level and discharge
     * @param list
     * @return
     */
    public StringBuffer getQFHF(List list){
        StringBuffer sb = new StringBuffer();        
        Feature disfe = (Feature) list.get(0);
        List listQF = (List)disfe.getProperty("ParamQF");

        StringWriter stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(stringWriter);
        String format="%10d%10.1f%10.2f";
        Object[] o;
        for (int i = 0; i < listQF.size(); i++) {
	        Feature qfFe = (Feature)listQF.get(i);
	        i = i+1;
	        if (i >= 2)i=i+1;
	        o = new Object[]{
	                new Integer(i),
	                new Double( Double.parseDouble(""+qfFe.getProperty("qf")) ),
	                new Double( Double.parseDouble(""+qfFe.getProperty("qdir")) ),
	        };
	        printWriter.printf(Locale.ENGLISH,format+"\n", o);
        }
        
        String names = "         j        qf      qdir";
        sb.append(names+"\n");
        sb.append(stringWriter.getBuffer());

        sb.append("          IHGEN Zeilen, die an der jeweiligen Kontinuitaetslinie j"+"\n");
        sb.append("            eine Wasserspiegelhoehe hf vorgeben"+"\n");

        sb.append("         j        hf"+"\n");        
        StringWriter stringWriter2 = new StringWriter();
        PrintWriter printWriter2 = new PrintWriter(stringWriter2);
        String format2 = "%10d%10.2f";

        Object[] o2 = new Object[]{
                new Integer(2),
                new Double( Double.parseDouble(""+disfe.getProperty("ParamHF")) ),
        };
        printWriter2.printf(Locale.ENGLISH,format2+"\n", o2);

        sb.append(stringWriter2.getBuffer());
        sb.append("x"+"\n");
        
        return sb;
    }
    
    /**
     * gets water level and discharge
     * @param list
     * @return
     */
    public StringBuffer getQFHFDyn(List list){
        StringBuffer sb = new StringBuffer();        
        Feature disfe = (Feature) list.get(0);
        List listQF = (List)disfe.getProperty("ParamQF");

        StringWriter stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(stringWriter);
        String format="%10d%10.2f%10.3f";
        Object[] o;
        for (int i = 0; i < listQF.size(); i++) {
	        Feature qfFe = (Feature)listQF.get(i);
	        i = i+1;
	        if (i >= 2)i=i+1;
	        o = new Object[]{
	                new Integer(i),
	                new Double( Double.parseDouble(""+qfFe.getProperty("qf")) ),
	                new Double( Double.parseDouble(""+qfFe.getProperty("qdir")) ),
	        };
	        printWriter.printf(Locale.ENGLISH,format+"\n", o);
        }
        
        String names = "         j        qf      qdir";
        sb.append(names+"\n");
        sb.append(stringWriter.getBuffer());

        sb.append("          IHGEN Zeilen, die an der jeweiligen Kontinuitaetslinie j"+"\n");
        sb.append("            eine Wasserspiegelhoehe hf vorgeben"+"\n");

        sb.append("         j        hf"+"\n");        
        StringWriter stringWriter2 = new StringWriter();
        PrintWriter printWriter2 = new PrintWriter(stringWriter2);
        String format2 = "%10d%10.2f";

        Object[] o2 = new Object[]{
                new Integer(2),
                new Double( Double.parseDouble(""+disfe.getProperty("ParamHF")) ),
        };
        printWriter2.printf(Locale.ENGLISH,format2+"\n", o2);

        sb.append(stringWriter2.getBuffer());
        sb.append("x"+"\n");
        
        return sb;
    }
}
