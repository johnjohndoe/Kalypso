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
package org.kalypso.convert.model2d;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;

/**
 * this class converts the femXMLFile...
 * 
 * Created on 16.12.2004
 * 
 * @author Katharina Lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 */
public class AsciiFactory {

	final File exeDir;

	final String BC_File = "bc_2204.txt";

	final String MODELL_FILE = "model_tmp.2d";

	public AsciiFactory(File exeDir) {
		this.exeDir = exeDir;
	}

	/**
	 * creates the .2d asci file
	 * 
	 * @param sbFP
	 * @param sbAR
	 * @param sbFE
	 * @param sbRK
	 */
	public void createAsciiFile(StringBuffer sbFP, StringBuffer sbAR,
			StringBuffer sbFE, StringBuffer sbRK) {
		try {
			StringBuffer sbALL = new StringBuffer();
			sbALL.append(sbFP.toString());
			sbALL.append(sbAR.toString());
			sbALL.append(sbFE.toString());
			sbALL.append(sbRK.toString());
			OutputStreamWriter writer = new OutputStreamWriter(
					new FileOutputStream(new File(exeDir, MODELL_FILE)
					//							"C:\\Programme\\eclipse\\workspace\\Kalypso2d\\src\\org\\kalypso\\calc2d\\start\\model_tmp.2d")
					), "UTF-8");
			//							"\\org\\kalypso\\calc2d\\start\\model_tmp.2d"),"UTF-8");
			//							"C:\\Programme\\eclipse\\workspace\\Kalypso2d\\sim\\start2\\model_2204.2d"),"UTF-8");
			writer.write(sbALL.toString());
			writer.close();

		} catch (Exception ex) {
			System.out.println("error occurred in creating test file ");
		}
	}

	/**
	 * creates ascii file of boundary conditions
	 * 
	 * @param sbGeneral
	 */
	public void createBCAsciiFile(StringBuffer sbGeneral,
			StringBuffer general2SB, StringBuffer geomSB,
			StringBuffer iterationSB, StringBuffer viscSB, StringBuffer lineSB,
			StringBuffer sbDis, StringBuffer sbDyn) {
		try {
			StringBuffer sbALL = new StringBuffer();
			sbALL.append("Start Boundary Conditions" + "\n");
			sbALL.append("x" + "\n");
			sbALL.append(sbGeneral.toString());
			//		    sbALL.append("x"+"\n");
			sbALL.append(general2SB.toString());
			sbALL.append(geomSB.toString());
			sbALL.append(iterationSB.toString());
			sbALL.append(viscSB.toString());
			sbALL.append(lineSB.toString());
			sbALL.append(sbDis.toString());
			sbALL.append(sbDyn.toString());

			OutputStreamWriter writer = new OutputStreamWriter(
					new FileOutputStream(new File(exeDir, BC_File))
					//							"C:\\Programme\\eclipse\\workspace\\Kalypso2d\\src\\org\\kalypso\\calc2d\\start\\bc_2204.txt"),
					//							"C:\\Programme\\eclipse\\workspace\\Kalypso2d\\sim\\start2\\bc_2204.txt"),
					, "UTF-8");
			writer.write(sbALL.toString());
			writer.close();
		} catch (Exception ex) {
			System.out.println("error occurred in creating test file ");
		}
	}

}
