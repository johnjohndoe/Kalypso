/*
 * Created on 20.01.2005
 */
package org.kalypso.convert.model2d;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_LineString;
import org.deegree.model.geometry.GM_Position;
import org.kalypso.ogc.gml.serialize.GmlSerializer;

/**
 * ----------------------------------------------------------------------
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 */
public class LineBC {
    
    
    /**
     * Constructor
     */
    public LineBC(){
        
    }
    
    /**
     * creates continuity line of boundary conditions
     * @param ws
     * @param rootFeature
     */
    public StringBuffer createLineBC(GMLWorkspace ws, Feature rootFeature, String file2d, String schema2d){
        Feature lineCollectionFE = ws.resolveLink(rootFeature, "lineCollectionMember");
        List list = (List) lineCollectionFE.getProperty("lineMember");
        StringBuffer sb = new StringBuffer();
        sb.append("           read(iin,'(60i5)')(line(j,k),k=1,60)"+"\n");
        sb.append("   NCL Kontinuitaetslinien (d.h. Knotennummern der Linien)"+"\n");
      
        try {
	        URL gmlURL = new File(file2d).toURL();
	        URL schemaUrl = new URL(schema2d);
	        GMLWorkspace ws2d = GmlSerializer.createGMLWorkspace(gmlURL, schemaUrl);
	        final Feature rootFeature2d = ws2d.getRootFeature();
	        
	        for (int i = 0; i < list.size(); i++) {
		        StringBuffer sb2 = new StringBuffer();
	            Feature fe = (Feature)list.get(i);
	            if(fe != null){
		            GM_Curve curve = (GM_Curve)fe.getProperty("listLinePos");
	                GM_LineString asLineString = curve.getAsLineString();
	                GM_Position[] positions = asLineString.getPositions();
	                
	                StringWriter stringWriter = new StringWriter();
		            PrintWriter printWriter = new PrintWriter(stringWriter);
		            String format="%5d";
		            Object[] o;

	                for (int j = 0; j < positions.length; j++) {
	                    String s = positions[j].getX()+","+positions[j].getY()+" ";
	                    int id = getId(ws2d, rootFeature2d, positions[j].getX(),positions[j].getY(),0);
	                    
	                    o = new Object[]{
			                    new Integer( id ),
			            };
		                printWriter.printf(Locale.ENGLISH,format, o);
	                    
	                }

			        sb2.append(stringWriter.getBuffer());
		            printWriter.close();
		            try {
		                stringWriter.close();
		            } catch (IOException e) {
		                e.printStackTrace();
		            }
		            sb.append(sb2+"\n");
	            }
	        }
        } catch (Exception e) {
            System.out.println("error in generating continuity line");
            e.printStackTrace();
        }
        sb.append("            IQGEN Zeilen, die an der jeweiligen Kontinuitaetslinie j"+"\n");
        sb.append("            einen Durchfluss qf in der Richtung qdir vorgeben"+"\n");
        
        return sb;
    }
    
    /**
     * returns id of node with particular coordinates in continuity line
     * @param ws
     * @param rootFeature
     * @param x
     * @param y
     * @return
     */
    private int getId(GMLWorkspace ws, Feature rootFeature, double x, double y, double z){
        int id = 0;

        FEMNodes nodes = new FEMNodes();
        nodes.createNodeProperties(ws, rootFeature);
        ArrayList nodeList = nodes.getNodeList();
        
        for (Iterator iter = nodeList.iterator(); iter.hasNext();) {
            Node n = (Node) iter.next();
            double xNode = n.getX();
            double yNode = n.getY();
            if(xNode == x && yNode == y){
                id = n.getID();
                break;
            }
        }
        
        return id;
    }

}
