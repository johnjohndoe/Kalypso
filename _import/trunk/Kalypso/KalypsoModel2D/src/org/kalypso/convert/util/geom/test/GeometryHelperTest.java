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
 * Created on 07.02.2005
 */
package org.kalypso.convert.util.geom.test;

import junit.framework.TestCase;

import org.kalypso.convert.util.geom.GeometryHelper;

/**
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */
public class GeometryHelperTest extends TestCase {

    public void testGetAngle() {
        double[] v = new double[] { 0d, 12d };
        double[] w = new double[] { 012, 0d };
        System.out.println(Math.PI / 2 + " = " + GeometryHelper.getAngle(v, w)
                + " ?");
        System.out.println(Math.PI / 2 + " = " + GeometryHelper.getAngle(w, v)
                + " ?");
    }

    public void testGetNorm() {
        double result = GeometryHelper.getNorm(new double[] { 3, 4 });
        System.out.println("result:" + result);
        assertEquals("test ", 5d, result, 0);
    }

    public void testgetSpannedArea() {
        double[] v = new double[] { 2d, 12d, 0 };
        double[] w = new double[] { 12, 5d, 0 };
//        double[] vectorProduct = GeometryHelper.getVectorProduct(v, w);
        System.out.println("v,w:" + GeometryHelper.getVectorProduct(v, w)[2]);
        System.out.println("w,v:" + GeometryHelper.getVectorProduct(w, v)[2]);

        //        System.out.println(" area v w = "+ GeometryHelper.getSpannedArea(v,
        // w)+" ?");
        //        System.out.println(" area w v = "+ GeometryHelper.getSpannedArea(w,
        // v)+" ?");
    }

}