package org.kalypso.convert.model2d;

import java.io.FileOutputStream;
import java.io.OutputStreamWriter;


/**
 * this class converts the femXMLFile...
 * 
 * Created on 16.12.2004
 * 
 * @author Katharina Lupp<a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 */
public class AsciiFactory {


    /**
     * creates the .2d asci file 
     * @param sbFP
     * @param sbAR
     * @param sbFE
     * @param sbRK
     */
    public void createAsciiFile(StringBuffer sbFP, StringBuffer sbAR, StringBuffer sbFE, StringBuffer sbRK){
        try{
		    StringBuffer sbALL = new StringBuffer();
            sbALL.append(sbFP.toString());
            sbALL.append(sbAR.toString());
            sbALL.append(sbFE.toString());
            sbALL.append(sbRK.toString());
            
	        OutputStreamWriter writer =
//				new OutputStreamWriter( new FileOutputStream("d:\\projekte_tu\\test\\test.2d"), "UTF-8" );
//				new OutputStreamWriter( new FileOutputStream("C:\\Programme\\eclipse\\workspace\\Kalypso2d\\data\\istTest.2d"), "UTF-8" );
				new OutputStreamWriter( new FileOutputStream("C:\\Programme\\eclipse\\workspace\\Kalypso2d\\data\\test\\back2Ascii.2d"), "UTF-8" );
	        writer.write(sbALL.toString());
			writer.close();
      }catch(Exception ex){
          System.out.println("error occurred in creating test file ");
      }
    }
    
    /**
     * creates ascii file of boundary conditions
     * @param sbGeneral
     */
    public void createBCAsciiFile(StringBuffer sbGeneral, StringBuffer general2SB, StringBuffer geomSB, StringBuffer iterationSB,
            					  StringBuffer viscSB, StringBuffer lineSB, StringBuffer sbDis, StringBuffer sbDyn){
        try{
		    StringBuffer sbALL = new StringBuffer();
		    sbALL.append("Start Boundary Conditions"+"\n");
		    sbALL.append("x"+"\n");
            sbALL.append(sbGeneral.toString());
//		    sbALL.append("x"+"\n");
            sbALL.append(general2SB.toString());
            sbALL.append(geomSB.toString());
            sbALL.append(iterationSB.toString());
            sbALL.append(viscSB.toString());
            sbALL.append(lineSB.toString());
            sbALL.append(sbDis.toString());
            sbALL.append(sbDyn.toString());

            OutputStreamWriter writer =
				new OutputStreamWriter( new FileOutputStream("C:\\Programme\\eclipse\\workspace\\Kalypso2d\\data\\testBC.txt"), "UTF-8" );
	        writer.write(sbALL.toString());
			writer.close();
      }catch(Exception ex){
          System.out.println("error occurred in creating test file ");
      }
    }

}
