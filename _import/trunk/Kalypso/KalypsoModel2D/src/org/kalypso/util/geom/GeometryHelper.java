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
package org.kalypso.util.geom;

/**
 * 
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */
public class GeometryHelper {

    /**
     * 
     * @param v
     * @param w
     */
    public static double getAngle(double[] v, double w[]) {
        return Math.acos(getScalarProduct(v, w) / (getNorm(v) * getNorm(w)));
    }

    /**
     * 
     * @param v
     * @param w
     */
    public static double getSpannedArea(double[] v, double[] w) {
        return getNorm(getVectorProduct(v, w));
    }

    /**
     * 
     * @param v
     */
    public static double getNorm(double[] v) {
        double inner = 0d;
        for (int i = 0; i < v.length; i++)
            inner += Math.pow(v[i], 2d);
        return Math.pow(inner, 0.5d);
    }

    /**
     * 
     * @param v
     * @param w
     */
    public static double getScalarProduct(double[] v, double[] w) {
        double result = 0d;
        for (int i = 0; i < v.length; i++)
            result += v[i] * w[i];
        return result;
    }

    /**
     * 
     * @param a
     * @param b
     */
    public static double[] getVectorProduct(double[] a, double b[]) {
        double result[] = new double[a.length];
        result[0] = a[1] * b[2] - a[2] * b[1];
        result[1] = a[2] * b[0] - a[0] * b[2];
        result[2] = a[0] * b[1] - a[1] * b[0];
        return result;
    }

    /**
     * 
     * @param a
     * @param b
     */
    public static boolean isAngleClockwise(double[] a, double[] b) {
        return getVectorProduct(a, b)[2] > 0;
    }
}