/*
 * Created on 14.10.2004
 */
package org.kalypso.convert.model2d;

import java.util.HashMap;

/**
 * @author Katharina <a href="mailto:k.lupp@web.de>Katharina Lupp</a>
 *
 */
public class ConvertAsci2GML {
    
    private static String ns;
    private static String sl;
    private static String xmlSchema;
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
		        xmlFile.startFem2XML(xmlSchema, inFile, outFile, ns, sl);
		        boolean exists = xmlFile.existsDetailedFP();
		        System.out.println("exists: " + exists);
		        gmlInFile = outFile;
		        ConvertXML2GML gmlFile = new ConvertXML2GML();
		        gmlFile.startXML2GML(gmlInFile, gmlOutFile, ns, slGML, exists);
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
