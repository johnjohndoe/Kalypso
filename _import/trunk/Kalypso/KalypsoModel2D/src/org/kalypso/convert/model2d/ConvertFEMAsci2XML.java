package org.kalypso.convert.model2d;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
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
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 * 
 * @author Katharina Lupp<a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 * Created on 30.08.2004 
 */
public class ConvertFEMAsci2XML {
    
    //firstParamBlock
    private static String time;
    private static String iteration;
    private static String[] fParams;
    
    //FP
    private static String[] fp_id     = new String[100000];
    private static String[] fp_coord1 = new String[100000];
    private static String[] fp_coord2 = new String[100000];
    private static String[] fp_coord3 = new String[100000];
    
    //VA
    private static String[] va_id         = new String[100000];
    private static String[] va_x_velocity = new String[100000];
    private static String[] va_y_velocity = new String[100000];
    private static String[] va_riverDepth = new String[100000];
    private static String[] va_waterLevel = new String[100000];
    
    //GA
    private static String[] ga_id            = new String[100000];
    private static String[] ga_timeGradient1 = new String[100000];
    private static String[] ga_timeGradient2 = new String[100000];
    private static String[] ga_timeGradient3 = new String[100000];
    
    //VO
    private static String[] vo_id     = new String[100000];
    private static String[] vo_param1 = new String[100000];
    private static String[] vo_param2 = new String[100000];
    private static String[] vo_param3 = new String[100000];
    
    //GO 
    private static String[] go_id        = new String[100000];
    private static String[] go_timeStep1 = new String[100000];
    private static String[] go_timeStep2 = new String[100000];
    private static String[] go_timeStep3 = new String[100000];
    
    //ZU
    private static String[] zu_info1 = new String[100000];
    private static String[] zu_info2 = new String[100000];
    private static String[] zu_info3 = new String[100000];
    private static String[] zu_info4 = new String[100000];
    private static String[] zu_info5 = new String[100000];
    private static String[] zu_info6 = new String[100000];
    
    //AR
    private static String[] ar_id;
    private static String[] ar_p1;
    private static String[] ar_p2;
    private static String[] ar_eLeft;
    private static String[] ar_eRight;
    private static String[] ar_midSideNode;
    private static String[] tmpAR;
    
    //FE
    private static String[] fe_id;
    private static String[] fe_roughness;
    private static String[] fe_roughnessTimeStep;
    private static String[] fe_orderOfProcessing;
    private static String[] fe_defaultMass;
    private static String[] tmpFE;
    
    //RK
    private static String[] rk_idRoughness;
    private static String[] rk_roughness;
    private static String[] rk_color;
    private static String[] rk_param1 = new String[100000];
    private static String[] rk_param2 = new String[100000];
    private static String[] rk_param3 = new String[100000];
    private static String[] rk_param4 = new String[100000];
    private static String[] tmpRK = new String[100000];
    
    private static boolean detailedFP = false;
    
	//namespaces
    private static String NS;
    private static String GML_NS = "http://www.opengis.net/gml";
    private static String XLINK_NS = "http://www.w3.org/1999/xlink";
    private static String XSI_NS = "http://www.w3.org/2001/XMLSchema-instance";
    private static String XSI_schemaLocation_NS;
    
  
    /**
     * 
     * @param inFile
     * @return
     * @throws MalformedURLException
     * @throws FileNotFoundException
     * @throws IOException
     * @throws UnsupportedEncodingException
     * @throws SAXException
     */
    private static StringBuffer readAsciFile(String inFile)
		    throws MalformedURLException, FileNotFoundException, IOException,
		    UnsupportedEncodingException, SAXException {

		FileReader fileReader = new FileReader(inFile);
        LineNumberReader br = new LineNumberReader(fileReader);
        StringBuffer sb = new StringBuffer();
        String line = null;
        while ((line = br.readLine()) != null) {
             sb.append(line.trim() + "\n");
        }        
        fileReader.close();
        br.close();
		
		return sb;
    }
    
    /**
     * checks if start or result file of .2d
     * @return
     */
    public boolean existsDetailedFP(){
        return detailedFP;
    }

    /**
     * converts the first block with the parameters va, ga, vo, 
     * go and zu describing the feature point
     * 
     * @param s
     */
    private static void setFirstParamBlock(String s){
        int pos = s.indexOf("TI");
        int pos2 = s.indexOf("FP");
        String param = "";
        if (pos > 0){
	        param = s.substring(pos, pos2);
	        setTI(param);
        }
        
        String tmp = s.substring(pos2, s.length());
        fParams = tmp.split("FP");
        for(int i = 1; i < fParams.length; i++){
            pos = fParams[i].indexOf("VA");
            if (pos > 0){
                detailedFP = true;
		        param = fParams[i].substring(0, pos);
		        setFP(param, (i-1));
		        
		        pos2 = fParams[i].indexOf("GA");
		        param = fParams[i].substring(pos, pos2);
		        setVA(param, (i-1));
		        
		        pos = fParams[i].indexOf("VO");
		        param = fParams[i].substring(pos2, pos);
		        setGA(param, (i-1));
		        
		        pos2 = fParams[i].indexOf("GO");
		        param = fParams[i].substring(pos, pos2);
		        setVO(param, (i-1));
		        
		        pos = fParams[i].indexOf("ZU");
		        param = fParams[i].substring(pos2, pos);
		        setGO(param, (i-1));
		        
		        param = fParams[i].substring(pos, fParams[i].length());
		        setZU(param, (i-1));
            }else {
		        setFP(fParams[i], (i-1));
            }
        }
    }
    
    /**
     * converts the time and iteration param
     * @param s
     */
    private static void setTI(String s){
        String[]tmp = s.split("    ");
        for(int i = 0; i < tmp.length; i++){
            if (tmp[i]!=" "){
                time = tmp[2];
                iteration = tmp[4];
            }
        }
    }
    
    /**
     * gets the va values from the asci file
     * @param s
     * 
     */
    private static void setVA(String s, int i){
        StringTokenizer st = new StringTokenizer(s);
        while (st.hasMoreTokens()){
            String t = st.nextToken();
            va_id[i]   = st.nextToken();
            va_x_velocity[i] = st.nextToken();
            va_y_velocity[i] = st.nextToken();
            va_riverDepth[i] = st.nextToken();
            va_waterLevel[i] = st.nextToken();
            break;
        }
  
    }
    
    /**
     * gets the vo values from the asci file
     * @param s
     * 
     */
    private static void setVO(String s, int i){
        StringTokenizer st = new StringTokenizer(s);
        while (st.countTokens()> 0){
            String t     = st.nextToken();
            vo_id[i]     = st.nextToken();
            vo_param1[i] = st.nextToken();
            vo_param2[i] = st.nextToken();
            vo_param3[i] = st.nextToken();
        }
    }
    
    /**
     * gets the vo values from the asci file
     * @param s
     */
    private static void setZU(String s, int i){
        StringTokenizer st = new StringTokenizer(s);
        while (st.countTokens()> 0){
            String t = st.nextToken();
            zu_info1[i] = st.nextToken();
            zu_info2[i] = st.nextToken();
            zu_info3[i] = st.nextToken();
            zu_info4[i] = st.nextToken();
            zu_info5[i] = st.nextToken();
            zu_info6[i] = st.nextToken();
        }
    }
    
    /**
     * gets the go values from the asci file
     * @param s
     */
    private static void setGO(String s, int i){
        StringTokenizer st = new StringTokenizer(s);
        while (st.countTokens()> 0){
            String t = st.nextToken();
            go_id[i]   = st.nextToken();
            go_timeStep1[i] = st.nextToken();
            go_timeStep2[i] = st.nextToken();
            go_timeStep3[i] = st.nextToken();
        }
    }
    
    /**
     * gets the ga values from the asci file
     * @param s
     */
    private static void setGA(String s, int i){
        //optional parameters, default value = 0
        StringTokenizer st = new StringTokenizer(s);
        while (st.countTokens()> 0){
            if(st.hasMoreTokens() == true){
                String t = st.nextToken();
            } 
            if(st.hasMoreTokens() == true){
                ga_id[i] = st.nextToken();
            }else ga_id[i]= ""+0; 
            
            if(st.hasMoreTokens() == true){
                ga_timeGradient1[i] = st.nextToken();
            }else ga_timeGradient1[i]= ""+0; 
            
            if(st.hasMoreTokens() == true){
                ga_timeGradient2[i] = st.nextToken();
            }else ga_timeGradient2[i]= ""+0; 
            
            if(st.hasMoreTokens() == true){
                ga_timeGradient3[i] = st.nextToken();
            }else ga_timeGradient3[i]= ""+0;
            
        }
    }
    
    /**
     * gets the fp values from the asci file
     * @param fpString
     */
    private static void setFP(String fpString, int i){
        //parameters are mandatory
        if (fpString != null){
	        StringTokenizer st = new StringTokenizer(fpString);
	        while (st.countTokens() > 0){
	            fp_id[i]     = st.nextToken();
		        fp_coord1[i] = st.nextToken();
		        fp_coord2[i] = st.nextToken();
		        fp_coord3[i] = st.nextToken();
	        }
        } else System.out.println("No featurePoints are set. FeaturePoints are mandatory!");
    }    
   
    
    /**
     * gets the ar values from the asci file
     * @param arString
     */
    private static void setAR(String arString){
        tmpAR = arString.split("AR");
        ar_id = new String[tmpAR.length];
        ar_p1 = new String[tmpAR.length];
        ar_p2 = new String[tmpAR.length];
        ar_eLeft = new String[tmpAR.length];
        ar_eRight = new String[tmpAR.length];
        //optional parameter msn
        ar_midSideNode = new String[tmpAR.length];
        for(int i = 0; i < tmpAR.length; i++){
            String ar = tmpAR[i];
            StringTokenizer st = new StringTokenizer(ar);
            while (st.countTokens()> 0){
                ar_id[i]   = st.nextToken();
                ar_p1[i] = st.nextToken();
                ar_p2[i] = st.nextToken();
                ar_eLeft[i] = st.nextToken();
                ar_eRight[i] = st.nextToken();
                
                if(st.hasMoreTokens() == true){
                    ar_midSideNode[i] = st.nextToken();
                }else ar_midSideNode[i]= ""+0; 
            }
        }
    }
    
    /**
     * gets the fe values from the asci file
     * @param feString
     */
    private static void setFE(String feString){
       String t = feString;
       tmpFE = feString.split("FE");
       fe_id = new String[tmpFE.length];
       fe_roughness = new String[tmpFE.length];
       //optional parameter roughnessTimeStep, orderOfProcessing and defaultMass, 
       //default value = 0;
       fe_roughnessTimeStep = new String[tmpFE.length];
       fe_orderOfProcessing = new String[tmpFE.length];
       fe_defaultMass = new String[tmpFE.length];
       
       StringBuffer sb = new StringBuffer();

       for(int i = 0; i < tmpFE.length; i++){
           String fe = tmpFE[i]; 
           StringTokenizer st = new StringTokenizer(fe);
           while (st.countTokens()> 0){
               fe_id[i]   = st.nextToken();
               fe_roughness[i] = st.nextToken();
               
               if(st.hasMoreTokens() == true){
                   fe_roughnessTimeStep[i] = st.nextToken();
               }else fe_roughnessTimeStep[i]= ""+0; 
               
               if(st.hasMoreTokens() == true){
                   fe_orderOfProcessing[i] = st.nextToken();
               }else fe_orderOfProcessing[i]= ""+0; 
               
               if(st.hasMoreTokens() == true){
                   fe_defaultMass[i] = st.nextToken();
               }else fe_defaultMass[i]= ""+0; 
           }
       }
    }
    
    /**
     * gets the rk values from the asci file
     * @param rkString
     */
    private static void setRK(String rkString){
        tmpRK = rkString.split("RK");
        rk_idRoughness = new String[tmpRK.length];
        rk_roughness = new String[tmpRK.length];
        rk_color = new String[tmpRK.length];
        int count = 1;
        for(int i = 1; i < tmpRK.length; i++){
            String rk = tmpRK[i];
            StringTokenizer st = new StringTokenizer(rk);
            while (st.hasMoreTokens()){
                rk_idRoughness[i]   = st.nextToken();
                
                rk_roughness[i] = rk_idRoughness[i].substring(1,rk_idRoughness[i].length());
                
                if(!rk_roughness[i].equalsIgnoreCase("0")){
                    
	                rk_idRoughness[i] = rk_idRoughness[i].substring(0,1);
	                System.out.println(rk_idRoughness[i]+", "+ rk_roughness[i]+", "+rk_idRoughness[i]);
	                rk_color[i]  = st.nextToken();
	                
	                //optional parameters, default value = 0;
	                if(st.hasMoreTokens() == true){
	                    rk_param1[i]= st.nextToken();
	                }else rk_param1[i]= ""+0; 
	                
	                if(st.hasMoreTokens()== true){
	                    rk_param2[i]= st.nextToken();
	                }else rk_param2[i]= ""+0;
	                
	                if(st.hasMoreTokens()== true){
	                    rk_param3[i]= st.nextToken();
	                }else rk_param3[i]=""+0;
	                
	                if(st.hasMoreTokens()== true){
	                    rk_param4[i]= st.nextToken();
	                }else {
	                    rk_param4[i]= ""+0;
	                }
	                
//	                count++;
	                
		            break;
                    
                }
            }
        }
    }
    
    /**
     * creates the elements and attributes of FP
     * @param doc
     * @param efp
     */
    private static void createFPNode(Document doc, Element efp, int i){
        Element efp_id = doc.createElement("id");
        String fpid = "" + (i-1);
         
        try{
	        efp_id.appendChild( XMLHelper.createTextNode( doc, fpid ));
	        efp.appendChild(efp_id);
	          
	        Element efp_geometry = doc.createElement("geometry");
	        Element srs = doc.createElement("srs");
	        srs.appendChild(XMLHelper.createTextNode(doc, "EPSG:31467"));
	        efp_geometry.appendChild(srs);
	        Element eCoord1 = doc.createElement("X");
	        Element eCoord2 = doc.createElement("Y");
	        Element eCoord3 = doc.createElement("Z");
	        String coord1 = fp_coord1[(i-1)];
	        String coord2 = fp_coord2[(i-1)];
	        String coord3 = fp_coord3[(i-1)];
	        
	        eCoord1.appendChild( XMLHelper.createTextNode( doc, coord1 ));
	        eCoord2.appendChild( XMLHelper.createTextNode( doc, coord2 ));
	        eCoord3.appendChild( XMLHelper.createTextNode( doc, coord3 ));
	        
	        efp_geometry.appendChild(eCoord1);
	        efp_geometry.appendChild(eCoord2);
	        efp_geometry.appendChild(eCoord3);
	        
	        efp.appendChild(efp_geometry);
        }catch(Exception ex){
            System.out.println("error in defining node.. ");
        }
    }
    
    /**
     * creates the nodes for the elements fp, va, ga, vo, go and zu
     * @param doc
     * @param ele
     * @param i
     */
    private static void createFPParamsNode(Document doc, Element ele, int i){
        Element ePoint = doc.createElement("fp");
        ele.appendChild(ePoint);
        createFPNode(doc, ePoint, i);

        if(detailedFP == true){
            i = i-1;
	        Element eva = doc.createElement("va");
	        Element ega = doc.createElement("ga");
	        Element evo = doc.createElement("vo");
	        Element ego = doc.createElement("go");
	        Element ezu = doc.createElement("zu");
	        ele.appendChild(eva);
	        ele.appendChild(ega);
	        ele.appendChild(evo);
	        ele.appendChild(ego);
	        ele.appendChild(ezu);	
	        try{
	            createVANode(doc, eva, i);
		        createGANode(doc, ega, i);
	            createVONode(doc, evo, i);
	            createGONode(doc, ego, i);
	            createZUNode(doc, ezu, i);
	        }catch(Exception ex){
	            System.out.println("error in creating xml file from ascii file");
	            ex.printStackTrace();
	        }
        }
    }
    
    /**
     * creates the subNodes for the elements info1 - info6 of the zu node
     * @param doc
     * @param ezu
     * @param i
     */
    private static void createZUNode(Document doc, Element ezu, int i){
        Element e_info1 = doc.createElement("info1");
        Element e_info2 = doc.createElement("info2");
        Element e_info3 = doc.createElement("info3");
        Element e_info4 = doc.createElement("info4");
        Element e_info5 = doc.createElement("info5");
        Element e_info6 = doc.createElement("info6");
       
        e_info1.appendChild( XMLHelper.createTextNode(doc, zu_info1[i]) );
        e_info2.appendChild( XMLHelper.createTextNode(doc, zu_info2[i]) );
        e_info3.appendChild( XMLHelper.createTextNode(doc, zu_info3[i]) );
        e_info4.appendChild( XMLHelper.createTextNode(doc, zu_info4[i]) );
        e_info5.appendChild( XMLHelper.createTextNode(doc, zu_info5[i]) );
        e_info6.appendChild( XMLHelper.createTextNode(doc, zu_info6[i]) );
        
        ezu.appendChild(e_info1);
        ezu.appendChild(e_info2);
        ezu.appendChild(e_info3);
        ezu.appendChild(e_info4);
        ezu.appendChild(e_info5);
        ezu.appendChild(e_info6);
    }
    
    /**
     * creates the elements and attributes of the node GO
     * 
     * @param doc
     * @param ego
     * @param i
     */
    private static void createGONode(Document doc, Element ego, int i){
        Element e_timeGradientOld1 = doc.createElement("TimeGradientOfFormerTimeWarps1");
        Element e_timeGradientOld2 = doc.createElement("TimeGradientOfFormerTimeWarps2");
        Element e_timeGradientOld3 = doc.createElement("TimeGradientOfFormerTimeWarps3");
       
        e_timeGradientOld1.appendChild( XMLHelper.createTextNode(doc, go_timeStep1[i]) );
        e_timeGradientOld2.appendChild( XMLHelper.createTextNode(doc, go_timeStep2[i]) );
        e_timeGradientOld3.appendChild( XMLHelper.createTextNode(doc, go_timeStep3[i]) );
        
        ego.appendChild(e_timeGradientOld1);
        ego.appendChild(e_timeGradientOld2);
        ego.appendChild(e_timeGradientOld3);
    }
    
    /**
     * creates the elements and attributes of the node GA
     * 
     * @param doc
     * @param ega
     * @param i
     */
    private static void createGANode(Document doc, Element ega, int i){
        Element e_timeGradient1 = doc.createElement("timeGradient1");
        Element e_timeGradient2 = doc.createElement("timeGradient2");
        Element e_timeGradient3 = doc.createElement("timeGradient3");
       
        e_timeGradient1.appendChild( XMLHelper.createTextNode(doc, ga_timeGradient1[i]) );
        e_timeGradient2.appendChild( XMLHelper.createTextNode(doc, ga_timeGradient2[i]) );
        e_timeGradient3.appendChild( XMLHelper.createTextNode(doc, ga_timeGradient3[i]) );
        
        ega.appendChild(e_timeGradient1);
        ega.appendChild(e_timeGradient2);
        ega.appendChild(e_timeGradient3);
    }
    
    /**
     * creates the elements and attributes of the node VO
     * 
     * @param doc
     * @param evo
     * @param i
     */
    private static void createVONode(Document doc, Element evo, int i){
        Element e_oldTimeGradient1 = doc.createElement("degreeOfFreedomOfOldTimeGradient1");
        Element e_oldTimeGradient2 = doc.createElement("degreeOfFreedomOfOldTimeGradient2");
        Element e_oldTimeGradient3 = doc.createElement("degreeOfFreedomOfOldTimeGradient3");
       
        e_oldTimeGradient1.appendChild( XMLHelper.createTextNode(doc, vo_param1[i]) );
        e_oldTimeGradient2.appendChild( XMLHelper.createTextNode(doc, vo_param2[i]) );
        e_oldTimeGradient3.appendChild( XMLHelper.createTextNode(doc, vo_param3[i]) );
        
        evo.appendChild(e_oldTimeGradient1);
        evo.appendChild(e_oldTimeGradient2);
        evo.appendChild(e_oldTimeGradient3);
    }
    
    /**
     * creates the elements and attributes of the node VA
     * 
     * @param doc
     * @param eva
     * @param i
     */
    private static void createVANode(Document doc, Element eva, int i){
        Element e_xVelocity  = doc.createElement("xVelocity");
        Element e_yVelocity  = doc.createElement("yVelocity");
        Element e_riverDepth = doc.createElement("riverDepth");
        Element e_waterLevel = doc.createElement("waterLevel");
        
        e_xVelocity.appendChild( XMLHelper.createTextNode(doc, va_x_velocity[i]) );
        e_yVelocity.appendChild( XMLHelper.createTextNode(doc, va_y_velocity[i]) );
        e_riverDepth.appendChild( XMLHelper.createTextNode(doc, va_riverDepth[i]) );
        e_waterLevel.appendChild( XMLHelper.createTextNode(doc, va_waterLevel[i]) );
        
        eva.appendChild(e_xVelocity);
        eva.appendChild(e_yVelocity);
        eva.appendChild(e_riverDepth);
        eva.appendChild(e_waterLevel);
    }
    /**
     * creates the elements and attributes of AR
     * @param doc
     * @param ear
     * @param i
     */
    private static void createARNode(Document doc, Element ear, int i){
        Element ear_id = doc.createElement("id");
        Element ear_p1 = doc.createElement("p1");
        Element ear_p2 = doc.createElement("p2");
        Element ear_eLeft = doc.createElement("e_left");
        Element ear_eRight = doc.createElement("e_right");
        Element ear_msn = doc.createElement("midSideNode");
        
        ear_id.appendChild( XMLHelper.createTextNode( doc, ar_id[i] ));
        ear_p1.appendChild( XMLHelper.createTextNode( doc, ar_p1[i] ));
        ear_p2.appendChild( XMLHelper.createTextNode( doc, ar_p2[i] ));
        ear_eLeft.appendChild( XMLHelper.createTextNode( doc, ar_eLeft[i] ));
        ear_eRight.appendChild( XMLHelper.createTextNode( doc, ar_eRight[i] ));
        ear_msn.appendChild( XMLHelper.createTextNode( doc, ar_midSideNode[i] ));
        
        ear.appendChild(ear_id);
        ear.appendChild(ear_p1);
        ear.appendChild(ear_p2);
        ear.appendChild(ear_eLeft);
        ear.appendChild(ear_eRight);
        ear.appendChild(ear_msn);
    }
    
    /**
     * creates the elements and attributes of FE
     * @param doc
     * @param efe
     * @param i
     */
    private static void createFENode(Document doc, Element efe, int i){
        Element efe_id = doc.createElement("id");
        Element efe_roughness = doc.createElement("roughnessOld");
        Element efe_rTimeStep = doc.createElement("roughnessInTimeWarp");
        Element efe_oop = doc.createElement("orderOfProcessing");
        Element efe_dm = doc.createElement("defaultMass");

        efe_id.appendChild( XMLHelper.createTextNode( doc, fe_id[i] ));
        efe_roughness.appendChild( XMLHelper.createTextNode( doc, fe_roughness[i] ));
        efe_rTimeStep.appendChild( XMLHelper.createTextNode( doc, fe_roughnessTimeStep[i] ));
        efe_oop.appendChild( XMLHelper.createTextNode( doc, fe_orderOfProcessing[i] ));
        efe_dm.appendChild( XMLHelper.createTextNode( doc, fe_defaultMass[i] ));
        
        efe.appendChild(efe_id);
        efe.appendChild(efe_roughness);
        efe.appendChild(efe_rTimeStep);
        efe.appendChild(efe_oop);
        efe.appendChild(efe_dm);
    }
    
    /**
     * creates the elements and attributes of FE
     * @param doc
     * @param efe
     * @param i
     */
    private static void createRKNode(Document doc, Element erk, int i){
        Element erk_id = doc.createElement("id");
        Element erk_roughness = doc.createElement("roughness");
        Element erk_color = doc.createElement("color");
        
        erk_id.appendChild( XMLHelper.createTextNode( doc, rk_idRoughness[i] ));
        erk_roughness.appendChild( XMLHelper.createTextNode( doc, rk_roughness[i] ));
        erk_color.appendChild( XMLHelper.createTextNode( doc, rk_color[i] ));
        
        erk.appendChild(erk_id);
        erk.appendChild(erk_roughness);
        erk.appendChild(erk_color);
        
        if (rk_param1[i]!=null){
            Element erk_param1 = doc.createElement("param1");
            erk_param1.appendChild( XMLHelper.createTextNode( doc, rk_param1[i] ));
            erk.appendChild(erk_param1);
        }
        if (rk_param2[i]!=null){
            Element erk_param2 = doc.createElement("param2");
            erk_param2.appendChild( XMLHelper.createTextNode( doc, rk_param2[i] ));
            erk.appendChild(erk_param2);
        }
        if (rk_param3[i]!=null){
            Element erk_param3 = doc.createElement("param3");
            erk_param3.appendChild( XMLHelper.createTextNode( doc, rk_param3[i] ));
            erk.appendChild(erk_param3);
        }
        
        if (rk_param4[i]!=null){
            Element erk_param4 = doc.createElement("param4");
            erk_param4.appendChild( XMLHelper.createTextNode( doc, rk_param4[i] ));
            erk.appendChild(erk_param4);
        }
    }
    
    /**
     * creates xml document with parameters from the fem asci file
     * @param sbAsciFile
     */
    private static void setXML(String outFile) {
        
        Document doc = XMLHelper.createDocument();
       try{
            Element e = doc.createElement("simulation2d");
            e.setAttributeNode(	XMLHelper.createAttribute( doc, "xmlns", NS));
            e.setAttributeNode(	XMLHelper.createAttribute( doc, "xmlns:gml", GML_NS));
            e.setAttributeNode(	XMLHelper.createAttribute( doc, "xmlns:xlink", XLINK_NS));
            e.setAttributeNode(	XMLHelper.createAttribute( doc, "xmlns:xsi", 	XSI_NS));
            e.setAttributeNode(	XMLHelper.createAttribute( doc, "xsi:schemaLocation", 	XSI_schemaLocation_NS));
            doc.appendChild(e);

            for (int i = 1; i < fParams.length; i++){
                Element elem = doc.createElement("fpInfo");
                e.appendChild(elem);
                createFPParamsNode(doc, elem, i);
            }
            
            for (int j = 1; j < tmpAR.length; j++){
                Element ear = doc.createElement("ar");
                e.appendChild(ear);
                createARNode(doc, ear, j);
            }

            for (int k = 1; k < tmpFE.length; ++k){
                Element efe = doc.createElement("fe");
                e.appendChild(efe);
                createFENode(doc, efe, k);
            }
            
            for (int l = 1; l < tmpRK.length; l++){
                Element elemRK = doc.createElement("rk");
                e.appendChild(elemRK);
                createRKNode(doc, elemRK, l);
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
    
    /**
     * 
     * @param xmlSchema
     * @param inFile
     * @param outFile
     * @param ns
     * @param sl
     */
    public void startFem2XML(String xmlSchema, String inFile, String outFile, String ns, String sl){
        try {
            NS = ns;
            XSI_schemaLocation_NS = sl;
            
            //reads 2d ascii file
            StringBuffer sb = readAsciFile(inFile);
            
            int pos = sb.indexOf("AR");
            String fp = sb.substring(0, pos);
            setFirstParamBlock(fp);
            
            int pos2 = sb.indexOf("FE");
            String ar = "";
            if( (sb.indexOf("FE")-pos) < 0 )ar = sb.substring(pos2, pos);
            if( (sb.indexOf("FE")-pos) >= 0 )ar = sb.substring(pos, pos2);
            setAR(ar);
             
            pos = sb.indexOf("RK");
            String fe = sb.substring(pos2, pos);
            setFE(fe);
            
            String rk = sb.substring(pos, sb.length());
            setRK(rk);
            
            setXML(outFile);

        } catch (Exception e) {
            System.out.println("Error in converting asci file to xml format:");
            e.printStackTrace();
            }

    }

}