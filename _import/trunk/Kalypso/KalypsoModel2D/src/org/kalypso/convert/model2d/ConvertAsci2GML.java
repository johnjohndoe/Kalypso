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
 * Created on 14.10.2004
 */
package org.kalypso.convert.model2d;

import java.util.HashMap;

/**
 * @author Katharina Lupp <a href="mailto:k.lupp@web.de>Katharina Lupp</a>
 *
 */
public class ConvertAsci2GML {
    
    private static String ns;
    private static String sl;
    private static String inFile;
    private static String outFile;
    private static String gmlInFile;
    private static String gmlOutFile;
    private static String slGML;
    
    /**
     * prints out helping information
     * @param n an abstract integer to specifiy the help-information
     */
    private static void usage(int n) {
        switch (n) {
        case 0:
            System.out
                    .println("usage: java ConvertFEMAsci2XML [-s GMLSchema -i inputFile -o outputFileDirectory]\n"
                            + "                      [ -d outputdir]\n"
                            + "                      [--help]\n"
                            + "\n"
                            + "    -s          GMLSchema File-directory, containing absolut paths and filenames\n"
                            + "    -i          inputFile-directory, containing absolut paths and filenames\n"
                            + "    -o          destination-directory, containing absolut paths and filenames\n"
                            + "    --help      shows this help.\n");
            break;
        case 1:
            System.out
                    .println("Try 'java ConvertFEMAsci2XML --help' for more information.");
            break;
        default:
            System.out
                    .println("Unknown usage: Try 'java ConvertFEMAsci2XML --help' for more information.");
            break;
        }
    }
    
    
    public static void main(String[] args) {
        HashMap argsmap = null;
        // no parameter given
        if (args.length == 0) {
            System.out.println("ConvertAsci2GML: missing arguments");
            System.exit(1);
        } else if (args[0].equals("--help")|| args[0].equals("-help")) {
            usage(0);
            System.exit(0);
        } else if (args.length >= 2) {
            // two or more parameter
            argsmap = new HashMap();
            for (int i = 0; i < args.length; i += 2) {
                argsmap.put(args[i], args[i + 1]);
            }
        }

        if (args[0].equalsIgnoreCase("ConvertAsci2GML"))
            usage(0);
        if (argsmap.get("-i") != null && argsmap.get("-o") != null
                && argsmap.get("-n") != null && argsmap.get("-g") != null) {
            HashMap argsmap2 = new HashMap();
            argsmap2.put("InFile", argsmap.get("-i"));
            argsmap2.put("OutFile", argsmap.get("-o"));
            
            argsmap2.put("GMLOutFile", argsmap.get("-g"));
            
            argsmap2.put("Namespace", argsmap.get("-n"));
            argsmap2.put("SchemaLocation", argsmap.get("-s"));
            argsmap2.put("SchemaLocationForGML", argsmap.get("-l"));
            
            inFile = argsmap2.get("InFile").toString();
            outFile = argsmap2.get("OutFile").toString();
            ns = argsmap2.get("Namespace").toString();
            sl = argsmap2.get("SchemaLocation").toString();
            slGML = argsmap2.get("SchemaLocationForGML").toString();
	        gmlOutFile = argsmap2.get("GMLOutFile").toString();
	        
	        try{	            
		        ConvertFEMAsci2XML xmlFile = new ConvertFEMAsci2XML();
		        xmlFile.startFem2XML(inFile, outFile, ns, sl);
		        boolean exists = xmlFile.existsDetailedFP();
		        System.out.println("exists: " + exists);
		        gmlInFile = outFile;
		        ConvertXML2GML gmlFile = new ConvertXML2GML();
		        gmlFile.startXML2GML(gmlInFile, gmlOutFile, exists);
		        System.out.println("EOF ConvertAsci2GML");
	        }catch(Exception ex){
	            System.out.println("Error occurred in ConvertAsci2GML: ");
	            ex.printStackTrace();
	        }
        }else{
            System.out.println("not all arguments are set");
        }
         
    }
}
