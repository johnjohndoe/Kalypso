/*
 * Created on 13.09.2004
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.convert.model2d;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.StringTokenizer;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

/**
 * @author Katharina Lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 * 
 * Preferences - Java - Code Style - Code Templates
 */
public class ConvertBC2XML {
    
    private static HashMap mapBC = new HashMap();
    private static String[][]firstParamGroup = new String [2][100];
    private static String[][]secondParamGroup = new String [2][100];
    private static String[][]thirdParamGroup = new String [2][100];
    
    private static String[][]fourthParamGroup = new String [100][8];
    private static String[] id = new String[100];
    private static String[] viscosity1 = new String[100];
    private static String[] viscosity2 = new String[100];
    private static String[] viscosity3 = new String[100];
    private static String[] viscosity4 = new String[100];
    private static String[] ks = new String[100];
    private static String[] abst = new String[100];
    private static String[] durchbaum = new String[100];
    
    private static ArrayList fifthsList = new ArrayList();
    private static String[] sixthParamBlock = new String[100];
    
    private static String[][] seventhParameterBlock = new String[3][100];
    private static String[][] block7 = new String[100][100];
    private static String[][]valueNCYC = new String[100][50];
    private static String[][]paramNCYC = new String[100][50];
    private static String iqgenId;
    private static String iqgenQF;
    private static String iqgenQDir;
    private static String ihgenId;
    private static String ihgenHF;
    
    //namespaces
    private static String NS = "http://elbe.wb.tu-harburg.de";
    private static String GML_NS = "http://www.opengis.net/gml";
    private static String XLINK_NS = "http://www.w3.org/1999/xlink";
    private static String XSI_NS = "http://www.w3.org/2001/XMLSchema-instance";
    private static String XSI_schemaLocation_NS = "";
    
    
    public static void createXML(String outFile) {
        //TODO  optionale parameter... 
        Document doc = XMLHelper.createDocument();
        //doc.createTextNode("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
       try{
            Element e = doc.createElement("boundaryConditions2d");
            e.setAttributeNode(	XMLHelper.createAttribute( doc, "xmlns", NS));
            e.setAttributeNode(	XMLHelper.createAttribute( doc, "xmlns:gml", GML_NS));
            e.setAttributeNode(	XMLHelper.createAttribute( doc, "xmlns:xlink", XLINK_NS));
            e.setAttributeNode(	XMLHelper.createAttribute( doc, "xmlns:xsi", 	XSI_NS));
            e.setAttributeNode(	XMLHelper.createAttribute( doc, "xsi:schemaLocation", 	XSI_schemaLocation_NS));
            doc.appendChild(e);
            
            Element elemFG = doc.createElement("FirstParamGroup");
            e.appendChild(elemFG);
            create1PGNode(doc, elemFG);
            
            Element elemSG = doc.createElement("SecondParamGroup");
            e.appendChild(elemSG);
            create2PGNode(doc, elemSG);
            
            Element elemTG = doc.createElement("ThirdParamGroup");
            e.appendChild(elemTG);
            create3PGNode(doc, elemTG);
            
            Element elem4G = doc.createElement("FourthParamGroup");
            e.appendChild(elem4G);
            for(int i = 0; i < fourthParamGroup.length; i++){
                if(id[i]!= null){
	                Element vis = doc.createElement("viscosity");
	                elem4G.appendChild(vis);
	                create4PGNode(doc, vis, i);
                }
            }
            
            Element elem5G = doc.createElement("FifthParamGroup");
            e.appendChild(elem5G);
            int size = (int)Math.round(Double.parseDouble(firstParamGroup[1][7]));
            for (int j = 0; j < size; j++){
                create5PGNode(doc, elem5G);
            }
            
            Element elem6G = doc.createElement("SixthParamGroup");
            e.appendChild(elem6G);
            create6PGNode(doc, elem6G);
            
            size = (int)Math.round(Double.parseDouble(thirdParamGroup[1][4]));
            for(int k = 8; k < 11; k+=2){
                Element elem7G = doc.createElement("SeventhParamGroup");
                e.appendChild(elem7G);
                create7PGNode(doc, elem7G, k); 
            }
            
    		OutputStreamWriter writer =
    			new OutputStreamWriter( new FileOutputStream(outFile), "UTF-8" );
            
            final Transformer t = TransformerFactory.newInstance().newTransformer();
         
            t.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
            t.setOutputProperty(OutputKeys.INDENT, "yes");
            
            t.transform( new DOMSource( e), new StreamResult( writer ) );
    		writer.close();
    		
        }catch(Exception ex){
            System.out.println("error in convert2GML:");
            ex.printStackTrace();
        }

       }
    
    /**
     * sets the xml document for the first parameter group
     * @param doc
     * @param element
     */
    public static void create1PGNode(Document doc, Element element){
        Element neElement = doc.createElement("NE");
        Element nmatElement = doc.createElement("NMAT");
        Element npxElement = doc.createElement("NPX");
        Element nbxElement = doc.createElement("NBX");
        Element nwidElement = doc.createElement("NWID");
        Element nsidElement = doc.createElement("NSID");
        Element iprtElement = doc.createElement("IPRT");
        Element nclElement = doc.createElement("NCL");
        Element iroElement = doc.createElement("IRO");
        Element iqgenElement = doc.createElement("IQGEN");
        Element istgenElement = doc.createElement("ISTGEN");
        Element idnoptElement = doc.createElement("IDNOPT");
        Element iwindElement = doc.createElement("IWIND");
        Element irslpElement = doc.createElement("IRSLP");
        Element ihgenElement = doc.createElement("IHGEN");
        Element ncflwElement = doc.createElement("NCFLW");
        Element ibgenElement = doc.createElement("IBGEN");
        
        element.appendChild(neElement);
        element.appendChild(nmatElement);
        element.appendChild(npxElement);
        element.appendChild(nbxElement);
        element.appendChild(nwidElement);
        element.appendChild(nsidElement);
        element.appendChild(iprtElement);
        element.appendChild(nclElement);
        element.appendChild(iwindElement);
        element.appendChild(iroElement);
        element.appendChild(irslpElement);
        element.appendChild(iqgenElement);
        element.appendChild(ihgenElement);
        element.appendChild(istgenElement);
        element.appendChild(ncflwElement);
        element.appendChild(idnoptElement);
        element.appendChild(ibgenElement);
        
        neElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][0] ));
        nmatElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][1] ));
        npxElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][2] ));
        nbxElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][3] ));
        nwidElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][4] ));
        nsidElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][5] ));
        iprtElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][6] ));
        nclElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][7] ));
        iwindElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][8] ));
        iroElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][9] ));
        irslpElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][10] ));
        iqgenElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][11] ));
        ihgenElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][12] ));
        istgenElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][13] ));
        ncflwElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][14] ));
        idnoptElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][15] ));
        ibgenElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][16] ));
        
        if(firstParamGroup[1].length > 17){ 
            if(firstParamGroup[1][17] != null){
	            Element femElement = doc.createElement("FEM");
	            element.appendChild(femElement);
	            femElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][17] ));
	        } else if (firstParamGroup[1][18] != null){
	            Element aussteuElement = doc.createElement("AUSSTEU");
	            element.appendChild(aussteuElement);
	            aussteuElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][18] ));
	        }else if (firstParamGroup[1][18] != null){
	            Element morphElement = doc.createElement("MORPH");
	            element.appendChild(morphElement);
	            morphElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][19] ));
	        } else if (firstParamGroup[1][18] != null){
	            Element iturbElement = doc.createElement("ITURB");
	            element.appendChild(iturbElement);
	            iturbElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][20] ));
	        } else if (firstParamGroup[1][18] != null){
	            Element pBottomElement = doc.createElement("P_BOTTOM");
	            element.appendChild(pBottomElement);
	            pBottomElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][21] ));
	        } else if (firstParamGroup[1][18] != null){
	            Element pPrandtlElement = doc.createElement("P_PRANDTL");
	            element.appendChild(pPrandtlElement);
	            pPrandtlElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][22] ));
	        } else if (firstParamGroup[1][18] != null){
	            Element mineddyElement = doc.createElement("MINEDDY");
	            element.appendChild(mineddyElement);
	            mineddyElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][23] ));
	        } 
	        
            if(firstParamGroup[1].length > 23){
		        for(int i = 24; i < firstParamGroup[1].length; i++){
		            Element nameElement = doc.createElement("NAME");
		            element.appendChild(nameElement);
		            nameElement.appendChild(XMLHelper.createTextNode( doc, firstParamGroup[1][i] ));
		        }
            }
        }
    }
    
    /**
     * sets the xml document for the second parameter group
     * @param doc
     * @param element
     */
    public static void create2PGNode(Document doc, Element element){
        Element omegaElement = doc.createElement("OMEGA");
        Element elevElement = doc.createElement("ELEV");
        Element xscaleElement = doc.createElement("XSCALE");
        Element zscaleElement = doc.createElement("ZSCALE");
        Element dsetElement = doc.createElement("DSET");
        Element dsetdElement = doc.createElement("DSETD");
        Element unomElement = doc.createElement("UNOM");
        Element hminElement = doc.createElement("HMIN");
        Element ascaleElement = doc.createElement("ASCALE");
        Element urfcElement = doc.createElement("URFC");
        
        element.appendChild(omegaElement);
        element.appendChild(elevElement);
        element.appendChild(xscaleElement);
        element.appendChild(zscaleElement);
        element.appendChild(dsetElement);
        element.appendChild(dsetdElement);
        element.appendChild(unomElement);
        element.appendChild(hminElement);
        element.appendChild(ascaleElement);
        element.appendChild(urfcElement);
        
        omegaElement.appendChild(XMLHelper.createTextNode( doc, secondParamGroup[1][0] ));
        elevElement.appendChild(XMLHelper.createTextNode( doc, secondParamGroup[1][1] ));
        xscaleElement.appendChild(XMLHelper.createTextNode( doc, secondParamGroup[1][2] ));
        zscaleElement.appendChild(XMLHelper.createTextNode( doc, secondParamGroup[1][3] ));
        dsetElement.appendChild(XMLHelper.createTextNode( doc, secondParamGroup[1][4] ));
        dsetdElement.appendChild(XMLHelper.createTextNode( doc, secondParamGroup[1][5] ));
        unomElement.appendChild(XMLHelper.createTextNode( doc, secondParamGroup[1][6] ));
        hminElement.appendChild(XMLHelper.createTextNode( doc, secondParamGroup[1][7] ));
        ascaleElement.appendChild(XMLHelper.createTextNode( doc, secondParamGroup[1][8] ));
        urfcElement.appendChild(XMLHelper.createTextNode( doc, secondParamGroup[1][9] ));
        
        if(secondParamGroup[1].length > 10){ 
	        for(int i = 10; i < secondParamGroup[1].length; i++){
	            Element nameElement = doc.createElement("NAME");
	            element.appendChild(nameElement);
	            nameElement.appendChild(XMLHelper.createTextNode( doc, secondParamGroup[1][i] ));
	        }
            
        }
    }
    
    
    /**
     * sets the xml document for the third parameter group
     * @param doc
     * @param element
     */
    public static void create3PGNode(Document doc, Element element){
        Element nitiElement = doc.createElement("NITI");
        Element mbandElement = doc.createElement("MBAND");
        Element ncycElement = doc.createElement("NCYC");
        Element deltElement = doc.createElement("DELT");
        Element liElement = doc.createElement("LI");
        Element itsiElement = doc.createElement("ITSI");
        Element jsplptElement = doc.createElement("JSPLPT");
        Element ihoeElement = doc.createElement("IHOE");
        Element idenElement = doc.createElement("IDEN");
        Element igravElement = doc.createElement("IGRAV");
        Element nitnElement = doc.createElement("NITN");
        Element nstartElement = doc.createElement("NSTART");
        Element isplptElement = doc.createElement("ISPLPT");
        Element iconvgElement = doc.createElement("ICONVG");
        
        element.appendChild(nitiElement);
        element.appendChild(nitnElement);
        element.appendChild(mbandElement);
        element.appendChild(nstartElement);
        element.appendChild(ncycElement);
        element.appendChild(deltElement);
        element.appendChild(liElement);
        element.appendChild(itsiElement);
        element.appendChild(isplptElement);
        element.appendChild(jsplptElement);
        element.appendChild(ihoeElement);
        element.appendChild(idenElement);
        element.appendChild(iconvgElement);
        element.appendChild(igravElement);

        nitiElement.appendChild(XMLHelper.createTextNode( doc, thirdParamGroup[1][0] ));
        nitnElement.appendChild(XMLHelper.createTextNode( doc, thirdParamGroup[1][1] ));
        mbandElement.appendChild(XMLHelper.createTextNode( doc, thirdParamGroup[1][2] ));
        nstartElement.appendChild(XMLHelper.createTextNode( doc, thirdParamGroup[1][3] ));
        ncycElement.appendChild(XMLHelper.createTextNode( doc, thirdParamGroup[1][4] ));
        deltElement.appendChild(XMLHelper.createTextNode( doc, thirdParamGroup[1][5] ));
        liElement.appendChild(XMLHelper.createTextNode( doc, thirdParamGroup[1][6] ));
        itsiElement.appendChild(XMLHelper.createTextNode( doc, thirdParamGroup[1][7] ));
        isplptElement.appendChild(XMLHelper.createTextNode( doc, thirdParamGroup[1][8] ));
        jsplptElement.appendChild(XMLHelper.createTextNode( doc, thirdParamGroup[1][9] ));
        ihoeElement.appendChild(XMLHelper.createTextNode( doc, thirdParamGroup[1][10] ));
        idenElement.appendChild(XMLHelper.createTextNode( doc, thirdParamGroup[1][11] ));
        iconvgElement.appendChild(XMLHelper.createTextNode( doc, thirdParamGroup[1][12] ));
        igravElement.appendChild(XMLHelper.createTextNode( doc, thirdParamGroup[1][13] ));
        
        if(thirdParamGroup[1].length > 14){
	        if(thirdParamGroup[1][14] != null){
	            Element ipascheElement = doc.createElement("IPASCHE");
	            element.appendChild(ipascheElement);
	            ipascheElement.appendChild(XMLHelper.createTextNode( doc, thirdParamGroup[1][14] ));
	        } 
	        for(int i = 15; i < thirdParamGroup[1].length; i++){
	            Element nameElement = doc.createElement("NAME");
	            element.appendChild(nameElement);
	            nameElement.appendChild(XMLHelper.createTextNode( doc, thirdParamGroup[1][i] ));
	        }
            
        }
    }
 
    
    /**
     * sets the xml document for the fourth parameter group
     * @param doc
     * @param element
     */
    public static void create4PGNode(Document doc, Element element, int i){
        Element nameElement = doc.createElement("Name");
        Element viscosity1Element = doc.createElement("WV1");
        Element viscosity2Element = doc.createElement("WV2");
        Element viscosity3Element = doc.createElement("WV3");
        Element viscosity4Element = doc.createElement("WV4");
        Element ksElement = doc.createElement("ks");
        
        element.appendChild(nameElement);
        element.appendChild(viscosity1Element);
        element.appendChild(viscosity2Element);
        element.appendChild(viscosity3Element);
        element.appendChild(viscosity4Element);
        element.appendChild(ksElement);
        
        nameElement.appendChild(XMLHelper.createTextNode( doc, id[i] ));
        viscosity1Element.appendChild(XMLHelper.createTextNode( doc, viscosity1[i] ));
        viscosity2Element.appendChild(XMLHelper.createTextNode( doc, viscosity2[i] ));
        viscosity3Element.appendChild(XMLHelper.createTextNode( doc, viscosity3[i] ));
        viscosity4Element.appendChild(XMLHelper.createTextNode( doc, viscosity4[i] ));
        ksElement.appendChild(XMLHelper.createTextNode( doc, ks[i] ));

        if (abst[i] != null){
	        Element abstElement = doc.createElement("abst");
	        element.appendChild(abstElement);
	        abstElement.appendChild(XMLHelper.createTextNode( doc, abst[i] ));
        }else if (durchbaum[i] != null){
	        Element durchbaumElement = doc.createElement("durchbaum");
	        element.appendChild(durchbaumElement);
	        durchbaumElement.appendChild(XMLHelper.createTextNode( doc, durchbaum[i] ));
        }
        
    }
    
    
    /**
     * sets the xml document for the fifth parameter group
     * @param doc
     * @param element
     */
    public static void create5PGNode(Document doc, Element element){
        Element nameElement = doc.createElement("line");
        element.appendChild(nameElement);
        StringBuffer sb = new StringBuffer(1000);
        for (int a = 0; a < fifthsList.size(); a++){
            sb.append(fifthsList.get(a)+" ");
        }
        nameElement.appendChild(XMLHelper.createTextNode( doc,  sb.toString()));
    }
    
    /**
     * sets the xml document for the sixth parameter group
     * 
     * @param doc
     * @param element
     */
    public static void create6PGNode(Document doc, Element element){
        Element qfElement = doc.createElement("ParamQF");
        Element hfElement = doc.createElement("ParamHF");
        element.appendChild(qfElement);
        element.appendChild(hfElement);
        
        Element nameElement = doc.createElement("Name");
        Element qf_Element = doc.createElement("qf");
        Element qdirElement = doc.createElement("qdir");
        qfElement.appendChild(nameElement);
        qfElement.appendChild(qf_Element);
        qfElement.appendChild(qdirElement);
      
        nameElement.appendChild(XMLHelper.createTextNode( doc,  sixthParamBlock[0]));
        qf_Element.appendChild(XMLHelper.createTextNode( doc,  sixthParamBlock[1]));
        qdirElement.appendChild(XMLHelper.createTextNode( doc,  sixthParamBlock[2]));
        hfElement.appendChild(XMLHelper.createTextNode( doc,  sixthParamBlock[4]));
    }
    
    /**
     * sets the xml document for the seventh parameter group
     * @param doc
     * @param element
     */
    public static void create7PGNode(Document doc, Element element, int k){
        Element nbxElement = doc.createElement("nbx");
        Element nsidElement = doc.createElement("nsid");
        Element deltElement = doc.createElement("delt");
        Element iqgenElement = doc.createElement("iqgen");
        Element ihgenElement = doc.createElement("ihgen");
        Element istgenElement = doc.createElement("istgen");
        Element ncflwElement = doc.createElement("ncflw");
        
        element.appendChild(nbxElement);
        element.appendChild(nsidElement);
        element.appendChild(deltElement);
        element.appendChild(iqgenElement);
        element.appendChild(ihgenElement);
        element.appendChild(istgenElement);
        element.appendChild(ncflwElement);
        
        nbxElement.appendChild(XMLHelper.createTextNode( doc,  valueNCYC[k][2]));
        nsidElement.appendChild(XMLHelper.createTextNode( doc,  valueNCYC[k][3]));
        deltElement.appendChild(XMLHelper.createTextNode( doc,  valueNCYC[k][4]));
        iqgenElement.appendChild(XMLHelper.createTextNode( doc,  valueNCYC[k][5]));
        ihgenElement.appendChild(XMLHelper.createTextNode( doc,  valueNCYC[k][6]));
        istgenElement.appendChild(XMLHelper.createTextNode( doc,  valueNCYC[k][7]));
        ncflwElement.appendChild(XMLHelper.createTextNode( doc,  valueNCYC[k][8]));
        
        if (valueNCYC[k][9] != null){
            Element idenIwindIbgenElement = doc.createElement("idenIwindIbgen");
	        element.appendChild(idenIwindIbgenElement);
	        idenIwindIbgenElement.appendChild(XMLHelper.createTextNode( doc,  valueNCYC[k][9]));
        }
        
        Element nclLineElement = doc.createElement("nclLine");
        element.appendChild(nclLineElement);
        
        Element qfElement = doc.createElement("ParamQF");
        Element hfElement = doc.createElement("ParamHF");
        nclLineElement.appendChild(qfElement);
        nclLineElement.appendChild(hfElement);
        
        Element nameElement = doc.createElement("Name");
        Element qf_Element = doc.createElement("qf");
        Element qdirElement = doc.createElement("qdir");
        qfElement.appendChild(nameElement);
        qfElement.appendChild(qf_Element);
        qfElement.appendChild(qdirElement);
      
        nameElement.appendChild(XMLHelper.createTextNode( doc,  block7[k][0]));
        qf_Element.appendChild(XMLHelper.createTextNode( doc,  block7[k][1]));
        qdirElement.appendChild(XMLHelper.createTextNode( doc,  block7[k][2]));
        hfElement.appendChild(XMLHelper.createTextNode( doc,  block7[k][4]));
    }


    /**
     * sets the StringBuffer with each param block
     * @param paramsBlock
     * @return
     */
    public static void setBCStringBuffer(String[] paramsBlock){
        HashMap map = new HashMap();
        firstParamGroup  = setFirstThreeParamGroups(paramsBlock[1], "NE");
        secondParamGroup = setFirstThreeParamGroups(paramsBlock[3], "OMEGA");
        thirdParamGroup  = setFirstThreeParamGroups(paramsBlock[4], "NITI");
        
        setFourthParamGroup(paramsBlock[5], "NMAT");
        
        int pos = paramsBlock[6].indexOf("IQGEN Zeilen");
        String fifthsParamBlock = paramsBlock[6].substring(0, pos);
        fifthsList = setFifthParamGroup(fifthsParamBlock);
        
        String sixthParams = paramsBlock[6].substring(pos, paramsBlock[6].length());
        sixthParamBlock = setSixthParamGroup(sixthParams);
        
     	int size = (int)Math.round(Double.parseDouble(thirdParamGroup[1][4]));
     	for (int i = 8; i < size; i+=2){
     	    setSeventhParamBlock(paramsBlock[i], i);
     	}
    }
    
        
    /**
     * sets the first three blocks of parameters. 
     * All three blocks have the same architecture with simple parameter/value pairs.
     * 
     * @param paramBlock
     * @param c
     * @return
     */
    public static String[][] setFirstThreeParamGroups(String paramBlock, String c){
       String[][] s = new String[2][100];
       StringTokenizer st = new StringTokenizer(paramBlock);
        while (st.hasMoreTokens()){
            String t = st.nextToken();
            if(t.startsWith( c )) {
                s = setParamGroup(t, st);
            }
        }
        return s;
    }
        
    
    /**
     * sets the parameters of the ParmeterGroups
     * 
     * @param firstParam
     * @param st
     * @return
     */
    public static String[][] setParamGroup(String firstParam, StringTokenizer st){
        
        int pgSize = (st.countTokens()+1)/2;
        String[][] paramGroup = new String [2][pgSize];
        paramGroup[0][0]= firstParam;
        
        for(int i = 1; i < pgSize; i++){
            String param = st.nextToken();
            paramGroup[0][i] = param;
        }
        for(int j = 0; j < pgSize; j++){
            String value = st.nextToken();
            paramGroup[1][j] = value;
        }
        
        return paramGroup;
    }
    
    /**
     * sets the fourth block of parameters with viscosity, etc.
     * @param paramsBlock
     * @param c
     */
    public static void setFourthParamGroup(String paramsBlock, String c){
        HashMap fourthBlock = new HashMap();
        int pos = paramsBlock.indexOf("1");
        String s = paramsBlock.substring(pos, paramsBlock.length());
        String[] tmp = s.split("        ");
        for (int i = 0; i < tmp.length; i++){ 
            StringTokenizer st = new StringTokenizer(tmp[i]);
            while (st.hasMoreTokens()){
                id[i] = st.nextToken();
                viscosity1[i] = st.nextToken();
                viscosity2[i] = st.nextToken();
                viscosity3[i] = st.nextToken();
                viscosity4[i] = st.nextToken();
                ks[i] = st.nextToken();
                if(st.hasMoreTokens() == true) {
                    abst[i] = st.nextToken();
                    fourthBlock.put("abst"+i, abst[i]);
                }
                if(st.hasMoreTokens() == true) {
                    durchbaum[i] = st.nextToken();
                    fourthBlock.put("durchbaum"+i, durchbaum[i]);
                }
            }
        }
    }
    
    /**
     * sets the fifth block of parameters.
     * This is just a list of id of the points building a line.
     * 
     * @param paramsBlock
     * @return
     */
    public static ArrayList setFifthParamGroup(String paramsBlock){
        int pos = paramsBlock.lastIndexOf("Linien)");
        String s = paramsBlock.substring(pos+7, paramsBlock.length());
        
        //TODO zeilenumbruch erkennbar machen... 
        ArrayList list = new ArrayList();
        StringTokenizer st = new StringTokenizer(s);
        while(st.hasMoreTokens()){
            list.add(st.nextToken());
        }
        return list;
    }
    
    
    /**
     * sets the sixths block.
     * It is divided in two parts. 
     * The first one is for the IQGEN and the second one for the IHGEN parameters.
     * 
     * @param paramsBlock
     */
    public static String[] setSixthParamGroup(String paramsBlock){
        String[] sixthBlock = new String[5];
        String[] tmp = paramsBlock.split("IHGEN");
        String iqgenId = "";
        String iqgenQF = "";
        String iqgenQDir = "";
        String ihgenId = "";
        String ihgenHF = "";
        
        int pos = tmp[0].lastIndexOf("qdir");
        String iqgen = tmp[0].substring(pos+4, tmp[0].length());
        StringTokenizer st = new StringTokenizer(iqgen);
        while(st.hasMoreTokens()){
            iqgenId = st.nextToken();
        	iqgenQF = st.nextToken();
        	iqgenQDir = st.nextToken();
        }
        
        pos = tmp[1].lastIndexOf("hf");
        String ihgen = tmp[1].substring(pos+2, tmp[1].length());
        st = new StringTokenizer(ihgen);
        while(st.hasMoreTokens()){
            ihgenId = st.nextToken();
        	ihgenHF = st.nextToken();
        	break;
        }
        sixthBlock[0] = iqgenId;
        sixthBlock[1] = iqgenQF;
        sixthBlock[2] = iqgenQDir;
        sixthBlock[3] = ihgenId;
        sixthBlock[4] = ihgenHF;
        
        return sixthBlock;
    }
    
    /**
     * sets the seventh param block. 
     * This one is divided in two parts. The first one is similar
     * to the first three blocks but there should be kept in mind the optional parameters.
     * The second block is similar to the sixth block.
     * 
     * @param paramsBlock
     * @return
     */
    public static void setSeventhParamBlock(String paramsBlock, int k){
        String[]firstSplit = paramsBlock.split("IQGEN");
        
        //first block
        int paramsPos = firstSplit[0].lastIndexOf("idenIwindIbgen");
        String params = "nbx" + firstSplit[0].substring(0, paramsPos+14);
        String values = firstSplit[0].substring(paramsPos+14, firstSplit[0].length());
        
        String[]tmp = params.split(" ");
        String[]tmp2 = values.split(" ");
        int c = 0;
        for(int i = 0; i < tmp.length; i++){
            if(!tmp[i].equalsIgnoreCase("")){
                paramNCYC[k][c] = tmp[i];
                c++;
           }
        }
        c = 0;
        for(int j = 0; j < tmp2.length; j++){
            if(!tmp2[j].equalsIgnoreCase("")){
                valueNCYC[k][c] = tmp2[j];
                c++;
           }
        }
        
        //second block 
        String[]seventhBBlock = setSixthParamGroup(firstSplit[1]);
        block7[k] = seventhBBlock; 
    }
    
    /**
     * gets the parameters of the seventh block
     * @return
     */
    public static String[][] get7Block(){
        return block7;
    }
    

    /**
     * reads the boundary conditions asci file
     * @param argsmap
     * @return
     * @throws MalformedURLException
     * @throws FileNotFoundException
     * @throws IOException
     * @throws UnsupportedEncodingException
     * @throws SAXException
     */
    public static StringBuffer readBCAsciFile(HashMap argsmap)
            throws MalformedURLException, FileNotFoundException, IOException,
            UnsupportedEncodingException, SAXException {
        String inputFile = argsmap.get("InputFile").toString();
        XSI_schemaLocation_NS = ".\\data\\bc.xsd";
        BufferedReader br = new BufferedReader(new FileReader(inputFile));
        StringBuffer sb = new StringBuffer();
        String line = null;
        while ((line = br.readLine()) != null) {
            sb.append(line + "\n");
        }
        return sb;
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
                        .println("usage: java ConvertBC2XML [-s XMLSchema -i inputFile -o outputFileDirectory]\n"
                                + "                      [ -d outputdir]\n"
                                + "                      [--help]\n"
                                + "\n"
                                + "    -i          inputFile-directory, containing absolut paths and filenames\n"
                                + "    -o          destination-directory, containing absolut paths and filenames\n"
                                + "    --help      shows this help.\n");
                break;
            case 1:
                System.out
                        .println("Try 'java ConvertBC2XML --help' for more information.");
                break;
            default:
                System.out
                        .println("Unknown usage: Try 'java ConvertFEMAsci2XML --help' for more information.");
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
            System.out.println("ConvertFEMAsci2XML: missing arguments");
            System.exit(1);
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
        if (args[0].equalsIgnoreCase("ConvertBC2XML"))
            usage(0);
        if (argsmap.get("-i") != null && argsmap.get("-o") != null) {
            HashMap argsmap2 = new HashMap();
            argsmap2.put("InputFile", argsmap.get("-i"));
            argsmap2.put("OutFileDirectory", argsmap.get("-o"));
            try {
                StringBuffer sb = readBCAsciFile(argsmap2);
                String[] paramsBlock = sb.toString().split("x");
                
                setBCStringBuffer(paramsBlock);
                createXML( (argsmap2.get("OutFileDirectory")).toString() );

                System.out.println("EOF");
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }
}