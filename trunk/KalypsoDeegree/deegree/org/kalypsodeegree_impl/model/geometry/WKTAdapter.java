/*----------------    FILE HEADER  ------------------------------------------

This file is part of deegree.
Copyright (C) 2001 by:
EXSE, Department of Geography, University of Bonn
http://www.giub.uni-bonn.de/exse/
lat/lon Fitzke/Fretter/Poth GbR
http://www.lat-lon.de

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

Andreas Poth
lat/lon Fitzke/Fretter/Poth GbR
Meckenheimer Allee 176
53115 Bonn
Germany
E-Mail: poth@lat-lon.de

Jens Fitzke
Department of Geography
University of Bonn
Meckenheimer Allee 166
53115 Bonn
Germany
E-Mail: jens.fitzke@uni-bonn.de

                 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.model.geometry;

import java.util.ArrayList;

import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_LineString;
import org.deegree.model.geometry.GM_MultiCurve;
import org.deegree.model.geometry.GM_MultiPoint;
import org.deegree.model.geometry.GM_MultiSurface;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Ring;
import org.deegree.model.geometry.GM_Surface;
import org.deegree.model.geometry.GM_SurfaceBoundary;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;
import org.opengis.cs.CS_CoordinateSystem;


/**
 * Adapter class for exporting deegree geometries to WKT and to wrap WKT code
 * geometries to deegree geometries.
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class WKTAdapter {
        
//    private static DecimalFormatSymbols dfs = new DecimalFormatSymbols();
//    private static  DecimalFormat frm = null;
//    static {
//        dfs.setDecimalSeparator( '.' );
//        frm = new DecimalFormat( "#.#########", dfs );
//    }

    
    /**
     
     *
     * @param wkt
     * @return the corresponding <tt>GM_Object</tt>
     * @throws GM_Exception if type unsupported or conversion failed
     */
    public static GM_Object wrap( String wkt, CS_CoordinateSystem crs ) throws GM_Exception {
        Debug.debugMethodBegin( "WKTAdapter", "wrap(String)" );

        GM_Object geo = null;
             
        if ( wkt == null ) {
            return null;
            //throw new GM_Exception( "can't create a geometry from a null-string" );
        } else if ( wkt.startsWith( "POINT" ) ) {
            geo = wrapPoint( wkt, crs );
        } else if ( wkt.startsWith( "LINE" ) ) {
            geo = wrapCurve( wkt, crs );
        } else if ( wkt.startsWith( "POLY" ) ) {
            geo = wrapSurface( wkt, crs );
        } else if ( wkt.startsWith( "MULTIPOINT" ) ) {
            geo = wrapMultiPoint( wkt, crs );
        } else if ( wkt.startsWith( "MULTILINE" ) ) {
            geo = wrapMultiCurve( wkt, crs );
        } else if ( wkt.startsWith( "MULTIPOLY" ) ) {
            geo = wrapMultiSurface( wkt, crs );
        }


        Debug.debugMethodEnd();
        return geo;
    }
    
    /**
     * @param geom geometry
     *
     * @return 
     */
    public static StringBuffer export( GM_Object geom ) throws GM_Exception {
        
        Debug.debugMethodBegin( "WKTAdapter", "export(GM_Object)" );
        
        StringBuffer sb = null;
        if ( geom instanceof GM_Point ) {
            sb = export( (GM_Point)geom );
        } else if ( geom instanceof GM_Curve ) {
            sb = export( (GM_Curve)geom );
        } else if ( geom instanceof GM_Surface ) {
            sb = export( (GM_Surface)geom );
        } else if ( geom instanceof GM_MultiPoint ) {
            sb = export( (GM_MultiPoint)geom );
        } else if ( geom instanceof GM_MultiCurve ) {
            sb = export( (GM_MultiCurve)geom );
        } else if ( geom instanceof GM_MultiSurface ) {
            sb = export( (GM_MultiSurface)geom );
        }
        
        Debug.debugMethodEnd();

        return sb;
    }

    /**
     * @param point point geometry
     *
     * @return 
     */
    private static StringBuffer export( GM_Point point ) {
        
        Debug.debugMethodBegin( "WKTAdapter", "export(GM_Point)" );
        
        StringBuffer sb = new StringBuffer( 50 );      
        sb.append("POINT(");
        double[] points = point.getAsArray();
        for(int i=0;i<points.length-1;i++) {
                sb.append(points[i]+" ");
            }
        sb.append(points[points.length-1]);
        sb.append(") ");  
        
        Debug.debugMethodEnd();

        return sb;
    }

    /**
     *
     * @param cur curve geometry
     *
     * @return 
     *
     * @throws GM_Exception 
     */
    private static StringBuffer export( GM_Curve cur ) throws GM_Exception {
        Debug.debugMethodBegin( "WKTAdapter", "export(GM_Curve)" );

        GM_LineString ls = cur.getAsLineString();
        
        StringBuffer sb = new StringBuffer( ls.getNumberOfPoints()*30 );
        sb.append( "LINESTRING(" );
        
        for (int i = 0; i < ls.getNumberOfPoints()-1; i++) {
            GM_Position pos = ls.getPositionAt( i );
            double[] positions = pos.getAsArray();        
            for(int j=0;j<positions.length-1;j++) {
               sb.append(positions[j]+" ");
            }    
            sb.append(positions[positions.length-1]+",");
        }
        GM_Position pos = ls.getPositionAt( ls.getNumberOfPoints()-1 );
        double[] tmp = pos.getAsArray();        
        for(int j=0;j<tmp.length-1;j++) {
           sb.append(tmp[j]+" ");
        }  
        sb.append(tmp[tmp.length-1]+")");
        
        Debug.debugMethodEnd();

        return sb;
    }

    /**
     * 
     *
     * @param sur
     *
     * @return 
     *
     */
    private static StringBuffer export( GM_Surface sur )  {
        Debug.debugMethodBegin( "WKTAdapter", "export(GM_Surface)" );        
        
        GM_SurfaceBoundary subo = sur.getSurfaceBoundary();
        GM_Ring exter = subo.getExteriorRing();
        GM_Ring[] inter = subo.getInteriorRings();
        
        StringBuffer sb = new StringBuffer( 10000 );
        sb.append( "POLYGON((" );
        // exterior ring
        GM_Position[] pos = exter.getPositions();
        for (int i = 0; i < pos.length-1; i++) {
            double[] positions = pos[i].getAsArray();        
            for(int j=0;j<positions.length-1;j++) {
               sb.append(positions[j]+" ");
            } 
            sb.append(positions[positions.length-1]+",");
        }
        double[] positions = pos[pos.length-1].getAsArray();
        for(int j=0;j<positions.length-1;j++) {
               sb.append(positions[j]+" ");
            } 
        sb.append(positions[positions.length-1]+")");
        //interior rings 
        if ( inter != null ) {
            for (int j = 0; j < inter.length; j++) {
                sb.append( ",(" );
                pos = inter[j].getPositions();
                for (int i = 0; i < pos.length-1; i++) {
                    double[] intPos = pos[i].getAsArray();        
                    for(int l=0;l<intPos.length-1;l++) {
                         sb.append(intPos[l]+" ");
                    } 
                    sb.append(intPos[intPos.length-1]+",");//                    
                }
                double[] intPos = pos[pos.length-1].getAsArray();
                    for(int l=0;l<intPos.length-1;l++) {
                        sb.append(intPos[l]+" ");
                    } 
                sb.append(intPos[intPos.length-1]+")");
            }
        }
        sb.append( ")" );

        Debug.debugMethodEnd();

        return sb;
    }

    /**
     * @param mp
     * @return
     */
    private static StringBuffer export( GM_MultiPoint mp ) {
        Debug.debugMethodBegin( "WKTAdapter", "export(GM_MultiPoint)" );
        
        StringBuffer sb = new StringBuffer( mp.getSize() * 30 );
        sb.append( "MULTIPOINT(" );
        for (int i = 0; i < mp.getSize()-1; i++) {
            GM_Point pt = mp.getPointAt( i );
            double[] points = pt.getAsArray();
            for(int j=0;j<points.length-1;j++) {
                 sb.append(points[j]+" ");
            }
        sb.append(points[points.length-1]);
        sb.append(",");  
        }
        GM_Point pt = mp.getPointAt( mp.getSize()-1 );
        double[] points = pt.getAsArray();
            for(int j=0;j<points.length-1;j++) {
                 sb.append(points[j]+" ");
            }
        sb.append(points[points.length-1]+")");

        Debug.debugMethodEnd();

        return sb;
    }

    /**
     *
     *
     * @param mc
     *
     * @return 
     *
     * @throws GM_Exception 
     */
    private static StringBuffer export( GM_MultiCurve mc ) throws GM_Exception {
        Debug.debugMethodBegin( "WKTAdapter", "export(GM_MultiCurve)" );

        StringBuffer sb = new StringBuffer( 10000 );
        sb.append( "MULTILINESTRING(" );
        
        for (int i = 0; i < mc.getSize()-1; i++) {
            String s = export( mc.getCurveAt( i ) ).toString();
            s = s.substring( 10, s.length() );
            sb.append( s ).append( "," );
        }
        String s = export( mc.getCurveAt( mc.getSize()-1 ) ).toString();
        s = s.substring( 10, s.length() );
        sb.append( s ).append( ")" );

        Debug.debugMethodEnd();

        return sb;
    }

    /**
     *
     *     
     * @param ms
     *
     * @return 
     *
     * @throws GM_Exception 
     */
    private static StringBuffer export( GM_MultiSurface ms ) throws GM_Exception {
        Debug.debugMethodBegin( "WKTAdapter", "export(GM_MultiSurface)" );

        StringBuffer sb = new StringBuffer( 10000 );
        sb.append( "MULTIPOLYGON(" );
        
        for (int i = 0; i < ms.getSize()-1; i++) {
            String s = export( ms.getSurfaceAt( i ) ).toString();
            s = s.substring( 7, s.length() );
            sb.append( s ).append( "," );
        }
        String s = export( ms.getSurfaceAt( ms.getSize()-1 ) ).toString();
        s = s.substring( 7, s.length() );
        sb.append( s ).append( ")" );

        Debug.debugMethodEnd();

        return sb;
    }

    /**
     * creates a GM_Point from a WKT.
     *
     * @param wkt a Point WKT
     */
    public static GM_Point wrapPoint( String wkt, CS_CoordinateSystem crs ) 
                                       throws GM_Exception {
        Debug.debugMethodBegin( "WKTAdapter", "wrapPoint" );

        wkt = wkt.trim();
        wkt = wkt.substring(6,wkt.length()-1);
        double[] tmp = StringExtend.toArrayDouble( wkt, " " );
        GM_Position pos = GeometryFactory.createGM_Position( tmp );
        GM_Point point = GeometryFactory.createGM_Point( pos, crs );            
        Debug.debugMethodEnd();
        return point;
    }

    /**
     * creates a GM_Curve from a WKT.
     *
     * @param wkt linestring a WKT
     */
    public static GM_Curve wrapCurve( String wkt, CS_CoordinateSystem crs ) 
                                       throws GM_Exception {
        Debug.debugMethodBegin( "WKTAdapter", "wrapCurve" );
        
        wkt = wkt.trim();
        wkt = wkt.substring(11, wkt.length()-1);
        String[] points = StringExtend.toArray( wkt, ",", false);            
        GM_Position[] pos = new GM_Position[points.length];
        for(int i = 0; i < points.length; i++)  {
            double[] tmp = StringExtend.toArrayDouble( points[i], " " ); 
            pos[i] = GeometryFactory.createGM_Position( tmp );
        }
        GM_Curve curve = GeometryFactory.createGM_Curve( pos, crs );        
         
        Debug.debugMethodEnd();
        return curve;
    }

    /** 
     * creates a GM_Surface
     *
     * @param wkt polygon WKT
     */
    public static GM_Surface wrapSurface( String wkt, CS_CoordinateSystem crs ) 
                                           throws GM_Exception {
        Debug.debugMethodBegin( "WKTAdapter", "wrapSurface" );
        
        wkt = wkt.trim();
       
        GM_Position[] ext = null;
        ArrayList inn = new ArrayList();                
        if ( wkt.indexOf( "((" ) > 0 ) {
            wkt = wkt.substring( 9,  wkt.length()-1 );
            int pos = wkt.indexOf( ")" );
            String tmp = wkt.substring( 0,  pos );
            //external ring
            String[] points = StringExtend.toArray( tmp, ",", false);
            ext = new GM_Position[points.length];     
            for(int i = 0; i < points.length; i++)  {
                double[] temp = StringExtend.toArrayDouble( points[i], " " );                 
                ext[i] = GeometryFactory.createGM_Position( temp );
            }
            if ( pos+3 < wkt.length() ) {
            wkt = wkt.substring( pos+3,  wkt.length() );
                while ( wkt.indexOf( ")" ) > 0 ) {
                    pos = wkt.indexOf( ")" );
                    tmp = wkt.substring( 0,  pos );
                    //internal ring(s)
                    points = StringExtend.toArray( tmp, ",", false);
                    GM_Position[] intern = new GM_Position[points.length];
                    for(int i = 0; i < points.length; i++)  {
                        double[] temp = StringExtend.toArrayDouble( points[i], " " ); 
                        intern[i] = GeometryFactory.createGM_Position( temp );
                    } 
                    inn.add(intern);
                    if ( pos+3 < wkt.length() ) {
                        wkt = wkt.substring( pos+3,  wkt.length() );
                    } else {
                        break;
                    }                    
                }
            }
        } 
        GM_Position[][] inner = null;
        if ( inn.size() > 0 ) {
            inner = (GM_Position[][])inn.toArray( new GM_Position[inn.size()][] );
        }
        GM_Surface sur = 
            GeometryFactory.createGM_Surface( ext, inner, new GM_SurfaceInterpolation_Impl(), crs );

        Debug.debugMethodEnd();
        return sur;
    }    

    /**
     * creates a GM_MultiPoint from a WKT
     *
     * @param wkt  multipoint WKT
     */
    public static GM_MultiPoint wrapMultiPoint( String wkt, CS_CoordinateSystem crs )
                                         throws GM_Exception {
        Debug.debugMethodBegin( "WKTAdapter", "wrapMultiPoint" );
       
        wkt = wkt.trim();
        wkt = wkt.substring( 11, wkt.length() -1 );
        String[] coords = StringExtend.toArray( wkt, ",", false);
        GM_Position[] pos = new GM_Position[coords.length];
        for(int i = 0; i < coords.length; i++)  {
            double[] temp = StringExtend.toArrayDouble( coords[i], " " ); 
            pos[i] = GeometryFactory.createGM_Position( temp );
        }
            
        GM_Point[] points = new GM_Point[ pos.length ];
        for (int i = 0; i < pos.length; i++) {
            points[i] = GeometryFactory.createGM_Point( pos[i], crs );
        }        
        GM_MultiPoint mp = GeometryFactory.createGM_MultiPoint( points );

        Debug.debugMethodEnd();
        return mp;
    }

    /**
     * creates a GM_MultiCurve from a WKT
     *
     * @param wkt a WKT
     */
    public static GM_MultiCurve wrapMultiCurve( String wkt, CS_CoordinateSystem crs )
                                         throws GM_Exception {
        Debug.debugMethodBegin( "WKTAdapter", "wrapMultiCurve" );
        
        ArrayList crvs = new ArrayList();

        wkt = wkt.trim();  
        int pos = wkt.indexOf( ")" );
        String tmp = wkt.substring( 17,  pos );
        String[] coords = StringExtend.toArray( tmp, ",", false);
        GM_Position[] posi = new GM_Position[coords.length];
        for(int i = 0; i < coords.length; i++)  {
            double[] temp = StringExtend.toArrayDouble( coords[i], " " ); 
            posi[i] = GeometryFactory.createGM_Position( temp );
        }
        crvs.add( GeometryFactory.createGM_Curve( posi, crs ) );
        wkt = wkt.substring( pos+3,  wkt.length()-1 );
        while ( wkt.indexOf( ")" ) > 0 ) {
            GM_Position[] posi2 = new GM_Position[coords.length];
            pos = wkt.indexOf( ")" );
            tmp = wkt.substring( 0,  pos );
            coords = StringExtend.toArray( tmp, ",", false);
            for(int i = 0; i < coords.length; i++)  {
                double[] temp = StringExtend.toArrayDouble( coords[i], " " ); 
                posi2[i] = GeometryFactory.createGM_Position( temp );
            }
            crvs.add( GeometryFactory.createGM_Curve( posi2, crs ) );
            if ( pos+3 < wkt.length() ) {
                wkt = wkt.substring( pos+3,  wkt.length() );
            } else {
                break;
            }
        }
        
        GM_Curve[] curves = (GM_Curve[])crvs.toArray( new GM_Curve[ crvs.size() ] );
        GM_MultiCurve mc = GeometryFactory.createGM_MultiCurve( curves );
        
        Debug.debugMethodEnd();
        return mc;
    }
    
    /**
     * creates a GM_MultiSurface from a WKT
     *
     * @param wkt a WKT
     */
    public static GM_MultiSurface wrapMultiSurface( String wkt, CS_CoordinateSystem crs )
                                                     throws GM_Exception {
        Debug.debugMethodBegin( "WKTAdapter", "wrapMultiSurface" );
        
        ArrayList srfcs = new ArrayList();

        wkt = wkt.substring( 13 );
        // for each polygon 
        while ( wkt.indexOf( "((" ) > -1) {
            GM_Position[] ext = null;
            ArrayList inn = new ArrayList();
            int pos1 = wkt.indexOf( "))" );
            String tmp = wkt.substring( 2, pos1+1 );
            //  exterior ring        
            int pos = tmp.indexOf( ")" );
            String tmp2 = tmp.substring( 0,  pos );
            String[] points = StringExtend.toArray( tmp2, ",", false);
            ext = new GM_Position[points.length];
            for(int i = 0; i < points.length; i++)  {
                double[] temp = StringExtend.toArrayDouble( points[i], " " ); 
                ext[i] = GeometryFactory.createGM_Position( temp );
            }
            if ( pos+3 < tmp.length() ) {
                tmp = tmp.substring( pos+3,  tmp.length() );
                // for each inner ring
                while ( tmp.indexOf( ")" ) > 0 ) {
                    pos = tmp.indexOf( ")" );
                    tmp2 = tmp.substring( 0,  pos );
                    points = StringExtend.toArray( tmp2, ",", false);
                    GM_Position[] intern = new GM_Position[points.length];
                    for(int i = 0; i < points.length; i++)  {
                        double[] temp = StringExtend.toArrayDouble( points[i], " " ); 
                        intern[i] = GeometryFactory.createGM_Position( temp );
                    } 
                    inn.add(intern);
                    if ( pos+3 < tmp.length() ) {
                        tmp = tmp.substring( pos+3,  tmp.length() );
                    } else {
                        break;
                    }
                }   
            }
            GM_Position[][] inner = null; 
            if ( inn.size() > 0 ) {
                inner = (GM_Position[][])inn.toArray( new GM_Position[inn.size()][] );
            }
            GM_Surface sur = 
                GeometryFactory.createGM_Surface( ext, inner, new GM_SurfaceInterpolation_Impl(), crs );
            srfcs.add( sur );
            wkt = wkt.substring( pos1+3 );
        } 
        GM_Surface[] surfaces = 
            (GM_Surface[])srfcs.toArray( new GM_Surface[srfcs.size()] );
        GM_MultiSurface ms = GeometryFactory.createGM_MultiSurface( surfaces );
        Debug.debugMethodEnd();
        return ms;
    }    
    
}