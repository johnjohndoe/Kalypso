/*
 * Created on 20.01.2005
 *
 */
package org.kalypso.convert.model2d;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;

/**
 * ----------------------------------------------------------------------
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */
public class ConvertBC2Ascii {
    
    //TODO locations of schema und gmlFile of 2d -> dynamic
    private static String gml2dFile = "./data/test/myMesh.gml";
    private static String gml2dSchema = "http://troubadix.wb.tu-harburg.de/lupp/2dgml.xsd";
    
    public static void convertBC2Ascii(HashMap map){
        try{
	        URL gmlURL = new File((String)map.get("InputFile")).toURL();
	        URL schemaUrl = new URL((String)map.get("SchemaLocation"));
	        GMLWorkspace ws = GmlSerializer.createGMLWorkspace(gmlURL, schemaUrl);
	        
	        final Feature rootFeature = ws.getRootFeature();
	        
	        GeneralBC generalBC = new GeneralBC();
	        StringBuffer generalSB = generalBC.createGeneralBC(ws, rootFeature);
	        
	        GeneralBC general2BC = new GeneralBC();
	        StringBuffer general2SB = general2BC.createGeneral2BC(ws, rootFeature);
	        
	        BCGeom geom = new BCGeom();
	        StringBuffer bcGeomSB = geom.createBCGeom(ws, rootFeature);
	        
	        IterationBC iteration = new IterationBC();
	        StringBuffer iterationSB = iteration.createIterationProp(ws, rootFeature);
	        
	        Viscosity viscosity = new Viscosity();
	        StringBuffer viscSB = viscosity.createViscosity(ws, rootFeature);
	        
	        LineBC line = new LineBC();
	        StringBuffer lineSB = line.createLineBC(ws, rootFeature, gml2dFile, gml2dSchema);
	        
	        DischargeBC discharge = new DischargeBC();
	        StringBuffer sbDis = discharge.createDischarge(ws, rootFeature);
	        
	        DynamicBC dynBC = new DynamicBC();
	        StringBuffer sbDyn = dynBC.createDynamicBC(ws, rootFeature);
	        
	        AsciiFactory fac = new AsciiFactory();
	        fac.createBCAsciiFile(generalSB, general2SB, bcGeomSB, iterationSB, viscSB, lineSB,
	                			sbDis, sbDyn);
	        
	    } catch (MalformedURLException urlEx) {
	        System.out.println("MalformedURLException");
	        urlEx.printStackTrace();
	    } catch (Exception ex) {
	        System.out.println("Exception in Inserting2dGML");
	        ex.printStackTrace();
	    }

    }
    
    /**
     * prints out helping information
     * 
     * @param n
     *            an abstract integer to specifiy the help-information
     */
    private static void usage(int n) {
        switch (n) {
        case 0:
            System.out
                    .println("usage: java ConvertBC2Ascii [-s XMLSchema -i inputFile]\n"
                            + "                      [--help]\n"
                            + "\n"
                            + "    -i          inputFile-directory, containing absolut paths and filenames\n"
                            + "    -s          schemaLocation\n"
                            + "    --help      shows this help.\n");
            break;
        case 1:
            System.out
                    .println("Try 'java ConvertBC2Ascii --help' for more information.");
            break;
        default:
            System.out
                    .println("Unknown usage: Try 'java ConvertBC2Ascii --help' for more information.");
            break;
        }
    }

    /**
     * 
     * @param args
     *            the command-line arguments
     */
    public static void main(String[] args) {
        HashMap argsmap = null;
        // no parameter given
        if (args.length == 0) {
            System.out.println("ConvertBC2Ascii: missing arguments");
            //System.exit(1);
        } else if (args[0].equals("--help") || args[0].equals("-help")) {
            usage(0);
            System.exit(0);
        } else if (args.length >= 2) {
            // two or more parameter
            argsmap = new HashMap();
            for (int i = 0; i < args.length; i += 2) {
                argsmap.put(args[i], args[i + 1]);
            }
        }
        if (args[0].equalsIgnoreCase("ConvertBC2Ascii"))
            usage(0);
        if (argsmap.get("-i") != null && argsmap.get("-s") != null) {
            HashMap argsmap2 = new HashMap();
            argsmap2.put("InputFile", argsmap.get("-i"));
            argsmap2.put("SchemaLocation", argsmap.get("-s"));
            try {
                convertBC2Ascii(argsmap2);
                System.out.println("EOF Kalypso2d ConvertBC2Ascii");
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

}
