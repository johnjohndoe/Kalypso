/*
 * Created on 07.02.2005
 */
package org.kalypso.util.geom.test;

import junit.framework.TestCase;

import org.kalypso.util.geom.GeometryHelper;

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
        double[] vectorProduct = GeometryHelper.getVectorProduct(v, w);
        System.out.println("v,w:" + GeometryHelper.getVectorProduct(v, w)[2]);
        System.out.println("w,v:" + GeometryHelper.getVectorProduct(w, v)[2]);

        //        System.out.println(" area v w = "+ GeometryHelper.getSpannedArea(v,
        // w)+" ?");
        //        System.out.println(" area w v = "+ GeometryHelper.getSpannedArea(w,
        // v)+" ?");
    }

}