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
 * Created on 29.10.2004
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.convert.model2d;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.StringTokenizer;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

/**
 * @author Katharina Lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 * 
 * Preferences - Java - Code Style - Code Templates
 */
public class ConvertBC2GML {
    
    private static HashMap mapBC = new HashMap();
    private static String[][]firstParamGroup = new String [2][100];
    private static String[][]firstNewParamGroup = new String [2][100];
    private static String[][]secondParamGroup = new String [2][100];
    private static String[][]thirdParamGroup = new String [2][100];
    private static boolean exists = false;
    
    private static String[][]fourthParamGroup = new String [100][8];
    private static String[] id = new String[100];
    private static ArrayList v1 = new ArrayList();
    private static ArrayList v2 = new ArrayList();
    private static ArrayList v3 = new ArrayList();
    private static ArrayList v4 = new ArrayList();
    private static ArrayList ks_ = new ArrayList();
    private static ArrayList abst_ = new ArrayList();
    private static ArrayList durchbaum_ = new ArrayList();
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
    private static int size7Group = 0;
    
    //namespaces
    private static String NS = "http://elbe.wb.tu-harburg.de/2dModel";
    private static String GML_NS = "http://www.opengis.net/gml";
    private static String XLINK_NS = "http://www.w3.org/1999/xlink";
    private static String XSI_NS = "http://www.w3.org/2001/XMLSchema-instance";
    private static String XSI_schemaLocation_NS = "http://elbe.wb.tu-harburg.de/2dModel D:\\gml\\bc_gml2.xsd";
    
    //TODO locations of schema und gmlFile of 2d -> dynamic
    private static String gml2dFile = "./data/test/myMesh.gml";
    private static String gml2dSchema = "http://troubadix.wb.tu-harburg.de/lupp/2dgml.xsd";
    
    
    public static void createGML(String outFile) {
        Document doc = XMLHelper.createDocument();
       try{
            Element e = doc.createElement("boundaryConditions");
            e.setAttributeNode(	XMLHelper.createAttribute( doc, "xmlns", NS));
            e.setAttributeNode(	XMLHelper.createAttribute( doc, "xmlns:gml", GML_NS));
            e.setAttributeNode(	XMLHelper.createAttribute( doc, "xmlns:xlink", XLINK_NS));
            e.setAttributeNode(	XMLHelper.createAttribute( doc, "xmlns:xsi", 	XSI_NS));
            e.setAttributeNode(	XMLHelper.createAttribute( doc, "xsi:schemaLocation", 	XSI_schemaLocation_NS));
            doc.appendChild(e);
            
            //FirstParamGroup
            Element generalElement = setGMLParamGroups(e, doc, "generalCollectionMember",
                    					"generalCollection","generalMember","general");
            create1PGNode(doc, generalElement);

            //new param group
            if(exists == true){
	            Element general2Element = setGMLParamGroups(e, doc, "genCollectionMember",
	                    					"genCollection","genMember","gen");
	            create1bPGNode(doc, general2Element);
            }
            
            //SecondParamGroup
            Element geomElement = setGMLParamGroups(e, doc, "geomCollectionMember", 
                    				"geomCollection","geomMember","geom");
            create2PGNode(doc, geomElement);
            
            //ThirdParamGroup
            Element iterationElement = setGMLParamGroups(e, doc, "iterationCollectionMember", 
                    					"iterationCollection", "iterationMember", "iteration");
            create3PGNode(doc, iterationElement);
            
            //FourthParamGroup
            setFourthGMLParamGroup(e, doc);
            
            //FifthsParamGroup
            setNCL_GMLParamGroup(e, doc);
  
            //SixthParamGroup
            Element dischargeElement = setGMLParamGroups(e, doc, "dischargeCollectionMember", 
                    					"dischargeCollection", "dischargeMember", "discharge");
            create6PGNode(doc, dischargeElement);
            
            //SeventhParamGroup
            set7GMLParamGroup(e, doc);
            
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
     * sets gml for first three param groups of boundary conditions
     */
    private static Element setGMLParamGroups(Element e, Document doc, 
            String collMem, String coll, String member, String fe){
        Element gcmElement = doc.createElement(collMem);
        e.appendChild(gcmElement);
        Element gcElement = doc.createElement(coll);
        gcmElement.appendChild(gcElement);
        Element gmElement = doc.createElement(member);
        gcElement.appendChild(gmElement);
        Element generalElement = doc.createElement(fe);
        gmElement.appendChild(generalElement);
        
        return generalElement;
    }
    
    /**
     * sets fourth gml parameter group of boundary conditions
     * @param e
     * @param doc
     */
    private static void setFourthGMLParamGroup(Element e, Document doc){
        Element viscosity_cmElement = doc.createElement("viscosityCollectionMember");
        e.appendChild(viscosity_cmElement);
        Element viscosity_cElement = doc.createElement("viscosityCollection");
        viscosity_cmElement.appendChild(viscosity_cElement);
        for(int i = 0; i < fourthParamGroup.length; i++){
            if(id[i]!= null){
                Element viscosity_mElement = doc.createElement("viscosityMember");
                viscosity_cElement.appendChild(viscosity_mElement);
                Element viscosityElement = doc.createElement("viscosity");
                viscosity_mElement.appendChild(viscosityElement);
                create4PGNode(doc, viscosityElement, i);
            }
        }
    }
    
    /**
     * sets dynamic boundary conditions
     * @param e
     * @param doc
     */
    private static void set7GMLParamGroup(Element e, Document doc){
        Element dynamicBC_cmElement = doc.createElement("dynamicBC_CollectionMember");
        e.appendChild(dynamicBC_cmElement);
        Element dynamicBC_cElement = doc.createElement("dynamicBC_Collection");
        dynamicBC_cmElement.appendChild(dynamicBC_cElement);
        
        int i = 8;
        if (thirdParamGroup[1][12].equalsIgnoreCase("0")) i = 7;
        for (int k = i; k < size7Group; k+=2){
            Element dynamicBC_mElement = doc.createElement("dynamicBC_Member");
            dynamicBC_cElement.appendChild(dynamicBC_mElement);
            Element dynamicBCElement = doc.createElement("dynamicBC");
            dynamicBC_mElement.appendChild(dynamicBCElement);
            create7PGNode(doc, dynamicBCElement, k); 
        }
    }
    
    /**
     * sets gml continuity lines 
     * @param e
     * @param doc
     */
    private static void setNCL_GMLParamGroup(Element e, Document doc){
        Element line_cmElement = doc.createElement("lineCollectionMember");
        e.appendChild(line_cmElement);
        Element line_cElement = doc.createElement("lineCollection");
        line_cmElement.appendChild(line_cElement);
        
        int size = (int)Math.round(Double.parseDouble(firstParamGroup[1][7]));
        for (int j = 0; j < size; j++){
            Element line_mElement = doc.createElement("lineMember");
            line_cElement.appendChild(line_mElement);
            Element lineElement = doc.createElement("line");
            line_mElement.appendChild(lineElement);
            //TODO NCL
            create5PGNode(doc, lineElement);
        }
    }
    
    /**
     * sets the xml document for the first parameter group
     * @param doc
     * @param element
     */
    private static void create1PGNode(Document doc, Element element){
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
    private static void create1bPGNode(Document doc, Element element){
        Element femElement = doc.createElement("FEM");
        Element aussElement = doc.createElement("AUSSTEU");
        Element morphElement = doc.createElement("MORPH");
        Element iturbElement = doc.createElement("ITURB");
        Element pBottElement = doc.createElement("P_BOTTOM");
        Element pPrandtlElement = doc.createElement("P_PRANDTL");
        Element mineddyElement = doc.createElement("MINEDDY");
        
        element.appendChild(femElement);
        element.appendChild(aussElement);
        element.appendChild(morphElement);
        element.appendChild(iturbElement);
        element.appendChild(pBottElement);
        element.appendChild(pPrandtlElement);
        element.appendChild(mineddyElement);
        
        femElement.appendChild(XMLHelper.createTextNode( doc, firstNewParamGroup[1][0] ));
        aussElement.appendChild(XMLHelper.createTextNode( doc, firstNewParamGroup[1][1] ));
        morphElement.appendChild(XMLHelper.createTextNode( doc, firstNewParamGroup[1][2] ));
        iturbElement.appendChild(XMLHelper.createTextNode( doc, firstNewParamGroup[1][3] ));
        pBottElement.appendChild(XMLHelper.createTextNode( doc, firstNewParamGroup[1][4] ));
        pPrandtlElement.appendChild(XMLHelper.createTextNode( doc, firstNewParamGroup[1][5] ));
        mineddyElement.appendChild(XMLHelper.createTextNode( doc, firstNewParamGroup[1][6] ));

    }
    
    /**
     * sets the xml document for the second parameter group
     * @param doc
     * @param element
     */
    private static void create2PGNode(Document doc, Element element){
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
    private static void create3PGNode(Document doc, Element element){
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
    private static void create4PGNode(Document doc, Element element, int i){
        Element nameElement = doc.createElement("id");
        Element viscosity1Element = doc.createElement("wv1");
        Element viscosity2Element = doc.createElement("wv2");
        Element viscosity3Element = doc.createElement("wv3");
        Element viscosity4Element = doc.createElement("wv4");
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
    private static void create5PGNode(Document doc, Element element){
        Element nameElement = doc.createElement("listLinePos");
        element.appendChild(nameElement);
        Element elem = doc.createElement("gml:LineString");
        nameElement.appendChild(elem);
        StringBuffer sb = new StringBuffer(1000);
        
        try {
	        URL gmlURL = new File(gml2dFile).toURL();
	        URL schemaUrl = new URL(gml2dSchema);

	        GMLWorkspace ws = GmlSerializer.createGMLWorkspace(gmlURL, schemaUrl);
	        final Feature rootFeature = ws.getRootFeature();
	        
	        for (int a = 0; a < fifthsList.size(); a++){
	            String nodeId = ""+fifthsList.get(a);
	            
	            FEMNodes nodes = new FEMNodes();
	            nodes.createNodeProperties(ws, rootFeature);
	            ArrayList nodeList = nodes.getNodeList();

	            int node = (int)Math.round(Double.parseDouble(nodeId));
	            node = node-1;
	            if(nodeList.get(node) != null){
			        Element coordElem = doc.createElement("gml:coord");
			        elem.appendChild(coordElem);
		            int id = (int)Double.parseDouble(nodeId);
		            Element xElem = doc.createElement("gml:X");
		            Element yElem = doc.createElement("gml:Y");
		            Element zElem = doc.createElement("gml:Z");
		            coordElem.appendChild(xElem);
		            coordElem.appendChild(yElem);
		            coordElem.appendChild(zElem);
	                Node n = (Node)nodeList.get(node);
	                xElem.appendChild(XMLHelper.createTextNode(doc, ""+n.getX()));
		            yElem.appendChild(XMLHelper.createTextNode(doc, ""+n.getY()));
		            zElem.appendChild(XMLHelper.createTextNode(doc, ""+n.getZ()));
	            }
	        }
        } catch (Exception e) {
            System.out.println("error in generating continuity lines");
            e.printStackTrace();
        }
    }
    
    /**
     * sets the xml document for the sixth parameter group
     * 
     * @param doc
     * @param element
     */
    private static void create6PGNode(Document doc, Element element){
        Element qfElement = doc.createElement("ParamQF");
        Element hfElement = doc.createElement("ParamHF");
        element.appendChild(qfElement);
        element.appendChild(hfElement);
        Element qfFeElement = doc.createElement("ParamQFFeature");
        qfElement.appendChild(qfFeElement);
        
        Element qf_Element = doc.createElement("qf");
        Element qdirElement = doc.createElement("qdir");
        qfFeElement.appendChild(qf_Element);
        qfFeElement.appendChild(qdirElement);
        
        qf_Element.appendChild(XMLHelper.createTextNode( doc,  sixthParamBlock[1]));
        qdirElement.appendChild(XMLHelper.createTextNode( doc,  sixthParamBlock[2]));
        hfElement.appendChild(XMLHelper.createTextNode( doc,  sixthParamBlock[4]));
    }
    
    /**
     * sets the xml document for the seventh parameter group
     * @param doc
     * @param element
     */
    private static void create7PGNode(Document doc, Element element, int k){
        Element nbxElement = doc.createElement("nbx");
        Element nsidElement = doc.createElement("nsid");
        Element deltElement = doc.createElement("delt");
        Element iqgenElement = doc.createElement("iqgen");
        Element ihgenElement = doc.createElement("ihgen");
        Element istgenElement = doc.createElement("istgen");
        
        element.appendChild(nbxElement);
        element.appendChild(nsidElement);
        element.appendChild(deltElement);
        element.appendChild(iqgenElement);
        element.appendChild(ihgenElement);
        element.appendChild(istgenElement);

        nbxElement.appendChild(XMLHelper.createTextNode( doc,  valueNCYC[k][1]));
        nsidElement.appendChild(XMLHelper.createTextNode( doc,  valueNCYC[k][2]));
        deltElement.appendChild(XMLHelper.createTextNode( doc,  valueNCYC[k][3]));
        iqgenElement.appendChild(XMLHelper.createTextNode( doc,  valueNCYC[k][4]));
        ihgenElement.appendChild(XMLHelper.createTextNode( doc,  valueNCYC[k][5]));
        istgenElement.appendChild(XMLHelper.createTextNode( doc,  valueNCYC[k][6]));
        
        Element ncflwElement = doc.createElement("ncflw");
        element.appendChild(ncflwElement);

        if (valueNCYC[k][7] == null || valueNCYC[k][7].indexOf("null")>-1){
            String s = ""+0;
            ncflwElement.appendChild(XMLHelper.createTextNode( doc,  s));
        }else ncflwElement.appendChild(XMLHelper.createTextNode( doc,  valueNCYC[k][7]));

        if (valueNCYC[k][8] != null){
            Element idenIwindIbgenElement = doc.createElement("idenIwindIbgen");
	        element.appendChild(idenIwindIbgenElement);
	        idenIwindIbgenElement.appendChild(XMLHelper.createTextNode( doc,  valueNCYC[k][8]));
        }
        
        Element nclLineElement = doc.createElement("nclType");
        element.appendChild(nclLineElement);
        
        Element discharge_cElement = doc.createElement("dischargeCollection");
        nclLineElement.appendChild(discharge_cElement);
        
        Element discharge_mElement = doc.createElement("dischargeMember");
        discharge_cElement.appendChild(discharge_mElement);
        Element dischargeElement = doc.createElement("discharge");
        discharge_mElement.appendChild(dischargeElement);
        
        Element qfElement = doc.createElement("ParamQF");
        Element hfElement = doc.createElement("ParamHF");
        dischargeElement.appendChild(qfElement);
        dischargeElement.appendChild(hfElement);

        Element qfFeElement = doc.createElement("ParamQFFeature");
        qfElement.appendChild(qfFeElement);
        
        Element qf_Element = doc.createElement("qf");
        Element qdirElement = doc.createElement("qdir");
        qfFeElement.appendChild(qf_Element);
        qfFeElement.appendChild(qdirElement);

        qf_Element.appendChild(XMLHelper.createTextNode( doc,  block7[k][1]));
        qdirElement.appendChild(XMLHelper.createTextNode( doc,  block7[k][2]));
        hfElement.appendChild(XMLHelper.createTextNode( doc,  block7[k][4]));
    }
     
    /**
     * sets the StringBuffer with each param block
     * @param paramsBlock
     * @return
     */
    private static void setBCStringBuffer(String[] paramsBlock){
        HashMap map = new HashMap();
        firstParamGroup  = setFirstThreeParamGroups(paramsBlock[0], "NE");
        if(paramsBlock[1].length() > 1){
	        firstNewParamGroup  = setFirstThreeParamGroups(paramsBlock[1], "FEM");
	        exists = true;
        }
        secondParamGroup = setFirstThreeParamGroups(paramsBlock[2], "OMEGA");
        thirdParamGroup  = setFirstThreeParamGroups(paramsBlock[3], "NITI");
        
        if(thirdParamGroup[1][12].equalsIgnoreCase("1")){
            
            setFourthParamGroup(paramsBlock[5], "NMAT");
	        
	        int pos = paramsBlock[6].indexOf("IQGEN Zeilen");
	        String fifthsParamBlock = paramsBlock[6].substring(0, pos);
	        fifthsList = setFifthParamGroup(fifthsParamBlock);
	        
	        String sixthParams = paramsBlock[6].substring(pos, paramsBlock[6].length());
	        sixthParamBlock = setSixthParamGroup(sixthParams);
	        
	     	size7Group = paramsBlock.length;
	     	for (int i = 8; i < paramsBlock.length; i+=2){
	     	    setSeventhParamBlock(paramsBlock[i], i);
	     	}
        }else {
	        setFourthParamGroup(paramsBlock[4], "NMAT");
	        
	        int pos = paramsBlock[5].indexOf("IQGEN Zeilen");
	        String fifthsParamBlock = paramsBlock[5].substring(0, pos);
	        fifthsList = setFifthParamGroup(fifthsParamBlock);
	        
	        String sixthParams = paramsBlock[5].substring(pos, paramsBlock[5].length());
	        sixthParamBlock = setSixthParamGroup(sixthParams);
	        
	     	size7Group = paramsBlock.length;
	     	for (int i = 7; i < paramsBlock.length; i+=2){
	     	    setSeventhParamBlock(paramsBlock[i], i);
	     	}
            
            
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
    private static String[][] setFirstThreeParamGroups(String paramBlock, String c){
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
    private static String[][] setParamGroup(String firstParam, StringTokenizer st){
        
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
    private static void setFourthParamGroup(String paramsBlock, String c){
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
                if(st.hasMoreTokens() == true)viscosity4[i] = st.nextToken();
                if(st.hasMoreTokens() == true)ks[i] = st.nextToken();
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
    private static ArrayList setFifthParamGroup(String paramsBlock){
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
     * The first one is for IQGEN and the second one for IHGEN parameters.
     * 
     * @param paramsBlock
     */
    private static String[] setSixthParamGroup(String paramsBlock){
        String[] sixthBlock = new String[5];
        String[] tmp = paramsBlock.split("IHGEN");
   
        int pos = tmp[0].lastIndexOf("qdir");
        String iqgen = tmp[0].substring(pos+4, tmp[0].length());
        StringTokenizer st = new StringTokenizer(iqgen);
        
        while(st.hasMoreTokens()){
            sixthBlock[0]=st.nextToken();
            sixthBlock[1]=st.nextToken();
            sixthBlock[2]=st.nextToken();
        }        
        pos = tmp[1].lastIndexOf("hf");
        String ihgen = tmp[1].substring(pos+2, tmp[1].length());
        st = new StringTokenizer(ihgen);
        while(st.hasMoreTokens()){
            sixthBlock[3]=st.nextToken();
            sixthBlock[4]=st.nextToken();
        	break;
        }
        
        return sixthBlock;
    }
    
    /**
     * sets the seventh param block. 
     * This one is divided in two parts. The first one is similar
     * to the first three blocks but there should be kept in mind optional parameters.
     * The second block is similar to the sixth block.
     * 
     * @param paramsBlock
     * @return
     */
    private static void setSeventhParamBlock(String paramsBlock, int k){
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
                System.out.println("paramNCYC[k][c]: "+paramNCYC[k][c]);
                c++;
           }
        }
        c = 0;
        for(int j = 0; j < tmp2.length; j++){
            if(!tmp2[j].equalsIgnoreCase("")){
                valueNCYC[k][c] = tmp2[j];
                System.out.println("valueNCYC[k][c]: "+valueNCYC[k][c]);
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
    private static String[][] get7Block(){
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
    private static StringBuffer readBCAsciFile(HashMap argsmap)
            throws MalformedURLException, FileNotFoundException, IOException,
            UnsupportedEncodingException, SAXException {
        
        String inputFile = argsmap.get("InputFile").toString();
        FileReader fileReader = new FileReader(inputFile);
        LineNumberReader br = new LineNumberReader(fileReader);
        StringBuffer sb = new StringBuffer();
        
        String line = null;
        while ((line = br.readLine()) != null) {
            if(line.length()>0)
            sb.append(line+"\n");
        }        
        fileReader.close();
        br.close();
        
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
                        .println("Unknown usage: Try 'java ConvertBC2GML --help' for more information.");
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
            System.out.println("ConvertBC2GML: missing arguments");
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
                int pos = sb.indexOf("NE");
                StringBuffer sb2 = new StringBuffer();
                sb2.append(sb.substring(pos, sb.length()));

                String[] paramsBlock = sb2.toString().split("x");
                setBCStringBuffer(paramsBlock);
                createGML( (argsmap2.get("OutFileDirectory")).toString() );

                System.out.println("EOF");
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }
}