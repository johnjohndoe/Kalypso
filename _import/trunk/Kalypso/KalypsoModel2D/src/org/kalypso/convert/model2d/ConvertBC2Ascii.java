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
 *
 */
package org.kalypso.convert.model2d;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

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
