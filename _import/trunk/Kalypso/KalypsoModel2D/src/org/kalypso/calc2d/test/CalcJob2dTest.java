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
 * Created on 23.03.2005
 *
 */
package org.kalypso.calc2d.test;

import java.io.File;

import junit.framework.TestCase;

import org.apache.commons.io.FileUtils;
import org.kalypso.calc2d.CalcJob2d;
import org.kalypso.calc2d.Constants2D;
import org.kalypso.services.calculation.service.CalcJobDataBean;

/**
 * 
 * -----------------------------------------------------------------
 * 
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */

public class CalcJob2dTest extends TestCase {
    public void test2dModell() throws Exception {
        final File baseDir = new File(
                "C:\\Programme\\KalypsoServer\\data\\tmp\\TEST");
        final File simDir = new File(baseDir, "sim");
        final File ergDir = new File(baseDir, "output");
        if (simDir.exists())
            FileUtils.cleanDirectory(simDir);
        if (ergDir.exists())
            FileUtils.cleanDirectory(ergDir);

        final CalcJobDataBean[] beans = new CalcJobDataBean[] {
                new CalcJobDataBean(Constants2D.MODELL_ID, "Modelldaten",
                        "calc/calcCaseResultMesh.gml"),

                new CalcJobDataBean(Constants2D.Boundary_ID, "Steuerdaten",
                        "calc/.boundaryConditions.gml"),

        };
        try {
            final CalcJob2d job = new CalcJob2d();
            job.run(baseDir, beans);
            if (job.isSucceeded())
                System.out.println("berechnung ohne Fehler beendet :-)");
            else
                System.out.println(":-( fehler irgendwo");
        } catch (Exception e) {
            e.printStackTrace();
            throw e;
        }
    }
}