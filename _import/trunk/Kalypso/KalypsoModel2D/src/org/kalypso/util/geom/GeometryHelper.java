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
     * @return
     */
    public static double getAngle(double[] v, double w[]) {
        return Math.acos(getScalarProduct(v, w) / (getNorm(v) * getNorm(w)));
    }

    /**
     * 
     * @param v
     * @param w
     * @return
     */
    public static double getSpannedArea(double[] v, double[] w) {
        return getNorm(getVectorProduct(v, w));
    }

    /**
     * 
     * @param v
     * @return
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
     * @return
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
     * @return
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
     * @return
     */
    public static boolean isAngleClockwise(double[] a, double[] b) {
        return getVectorProduct(a, b)[2] > 0;
    }
}