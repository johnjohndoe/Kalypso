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
 * 
 * Created on 02.11.2004
 *
 */
package org.kalypso.convert.model2d;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;

import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * This class is for inserting the 2d gml file into a <GMLWorkspace>and for
 * visualizing the mesh.
 * -----------------------------------------------------------------
 * 
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */
public class ConvertGML2Asci {

    /**
     * starts the insertion of the gml file in the kalypso environment
     * 
     * @param argsmap2
     */
    private static void convertGML2Asci(HashMap argsmap2) {
        try {
            URL gmlURL = new File((String) argsmap2.get("InputFile")).toURL();
//            URL schemaUrl = new URL((String) argsmap2.get("SchemaLocation"));
            URL schemaUrl = new File("C:/Programme/eclipse/workspace/Kalypso2d/data/schema/2dgml.xsd").toURL();
            
            GMLWorkspace ws = GmlSerializer.createGMLWorkspace(gmlURL, schemaUrl);

            final Feature rootFeature = ws.getRootFeature();
            
            //getting parameters defined in the nodeProperties
            FEMNodes nodes = new FEMNodes();
            nodes.createNodeProperties(ws, rootFeature);
            StringBuffer sb_fp = nodes.getSB_FP();
            ArrayList nodeList = nodes.getNodeList();
            
            //getting parameters defined in the meshProperties
            FEMMesh femMesh = new FEMMesh(nodeList);
            femMesh.getMeshProperties(ws, rootFeature);
            StringBuffer sb_rk = femMesh.getSB_RK();
            StringBuffer sb_fe = femMesh.getSB_FE();
            StringBuffer sb_ar = femMesh.getSB_AR();
            
	        //writing to .2d file in <AsciiFactory>
            AsciiFactory ascii = new AsciiFactory();
	        ascii.createAsciiFile(sb_fp, sb_ar, sb_fe, sb_rk);
      
        } catch (MalformedURLException urlEx) {
            System.out.println("MalformedURLException");
            urlEx.printStackTrace();
        } catch (Exception ex) {
            System.out.println("Exception in ConvertingGML2Asci");
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
                    .println("usage: java ConvertGML2Ascii [-s XMLSchema -i inputFile]\n"
                            + "                      [--help]\n"
                            + "\n"
                            + "    -i          inputFile-directory, containing absolut paths and filenames\n"
                            + "    -s          schemaLocation\n"
                            + "    --help      shows this help.\n");
            break;
        case 1:
            System.out
                    .println("Try 'java ConvertGML2Ascii --help' for more information.");
            break;
        default:
            System.out
                    .println("Unknown usage: Try 'java ConvertGML2Ascii --help' for more information.");
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
            System.out.println("ConvertGML2Ascii: missing arguments");
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
        if (args[0].equalsIgnoreCase("ConvertGML2Asci"))
            usage(0);
        if (argsmap.get("-i") != null && argsmap.get("-s") != null) {
            HashMap argsmap2 = new HashMap();
            argsmap2.put("InputFile", argsmap.get("-i"));
            argsmap2.put("SchemaLocation", argsmap.get("-s"));
            try {
                convertGML2Asci(argsmap2);
                System.out.println("EOF Kalypso2d ConvertGML2Ascii");
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

}