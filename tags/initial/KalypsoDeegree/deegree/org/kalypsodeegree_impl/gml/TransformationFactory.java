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
package org.deegree_impl.gml;

import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.deegree_impl.model.cs.GeographicCoordinateSystem;
import org.deegree_impl.model.cs.ProjectedCoordinateSystem;
import org.deegree_impl.model.ct.CoordinateTransformation;
import org.deegree_impl.model.ct.CoordinateTransformationFactory;
import org.deegree_impl.model.ct.MathTransform;
import org.deegree_impl.model.ct.MathTransformFactory;


/**
 * A Tranformation Factory: The <tt>Transform</tt> object, that this
 * class returns can be used for the transforming. Incoming Data can
 * either be <tt>CoordinateSystem</tt> or <tt>String</tt>.
 *
 * @version 1.1
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer</a>
 */
public class TransformationFactory {
    private ConvenienceCSFactoryFull csFactory = new ConvenienceCSFactoryFull();
    private CoordinateTransformationFactory trFactory;

    /**
     * default constructor: creates a CoordinateTransformationFactory.
     */
    public TransformationFactory() {
        trFactory = CoordinateTransformationFactory.getDefault();
    }

    /**
     * returns a <tt>Transform</tt> object that's able to perform a coordinate 
     * tranformation from the source- to the destination-coordinate system.
     * At the moment only transformations between geographic coordinate systems
     * and projected coordinated systems and vice versa are supported.
     *
     * @param src source coordinated system as a String
     * @param dest destination (target) coodinate system as a String
     * @return an instance of <tt>net.ct.Transform</tt> 
     * @exception Exception will be thrown if the desired transformaion isn't possible      
     */
    public MathTransform getTransform( String src, String dest ) throws Exception {
        try {
            CoordinateSystem sourceCS = csFactory.getCSByName( src );

            // System.out.println("\nsource CS:\n" + sourceCS);
            CoordinateSystem destinationCS = csFactory.getCSByName( dest );

            // System.out.println("\ndestinationCS:\n" + destinationCS);
            return getTransform( sourceCS, destinationCS );
        } catch ( Exception ex ) {
            System.out.println( ex );
            return null;
        }
    }

    /**
     * returns a <tt>Transform</tt> object that's able to perform a coordinate 
     * tranformation from the source- to the destination-coordinate system.
     * At the moment only transformations between geographic coordinate systems
     * geographic and projected coordinated systems and vice versa are
     * supported.
     *
     * @param src source coordinated system
     * @param dest destination (target) coodinate system
     * @return an instance of <tt>net.ct.Transform</tt> 
     * @exception Exception will be thrown if the desired transformaion isn't possible
     */
    public MathTransform getTransform( CoordinateSystem src, CoordinateSystem dest )
                               throws Exception {
        MathTransform finalTransform = null;

        if ( ( src instanceof GeographicCoordinateSystem && dest instanceof ProjectedCoordinateSystem ) ) {
            finalTransform = getGeogrToProjected( src, dest );
        } else if ( src instanceof GeographicCoordinateSystem && 
                        dest instanceof GeographicCoordinateSystem ) {
            CoordinateTransformation transformation = trFactory.createFromCoordinateSystems( src, 
                                                                                             dest );
            finalTransform = transformation.getMathTransform();
        } else if ( src instanceof ProjectedCoordinateSystem && 
                        dest instanceof ProjectedCoordinateSystem ) {
            finalTransform = getProjectedToProjected( src, dest );
        } else if ( src instanceof ProjectedCoordinateSystem && 
                        dest instanceof GeographicCoordinateSystem ) {
            finalTransform = getProjectedToGeogr( src, dest );
        } else {
            throw new Exception( "Not supported transformation!" );
        }

        return finalTransform;
    }

    /**
     * retuns a <tt>MathTransform</tt> object for the transformation of
     * coodinates from a geographic to a projected coodinate systems
     */
    private MathTransform getGeogrToProjected( CoordinateSystem src, CoordinateSystem dest )
                                       throws Exception {
        // get transfrom to EPSG:4326
        CoordinateSystem tmp = csFactory.getCSByName( "EPSG 4326" );
        CoordinateTransformation transformation = trFactory.createFromCoordinateSystems( src, tmp );
        MathTransform transform1 = transformation.getMathTransform();

        // get transfrom EPSG:4326 to projected coordinate system
        // (only tested for TransverseMecator)
        MathTransformFactory mtf = MathTransformFactory.getDefault();
        MathTransform transform2 = mtf.createParameterizedTransform( 
                                           ( (ProjectedCoordinateSystem)dest ).getProjection() );
        MathTransform finalTransform = mtf.createConcatenatedTransform( transform1, transform2 );
        return finalTransform;
    }

    /**
     * retuns a <tt>MathTransform</tt> object for the transformation of
     * coodinates between two projected coodinate systems
     */
    private MathTransform getProjectedToProjected( CoordinateSystem src, CoordinateSystem dest )
                                           throws Exception {
        // get transfrom of src to EPSG:4326
        CoordinateSystem tmp = csFactory.getCSByName( "EPSG 4326" );
        CoordinateTransformation transformation = trFactory.createFromCoordinateSystems( tmp, src );
        MathTransform transform1 = transformation.getMathTransform().inverse();

        // get transfrom EPSG:4326 to projected coordinate system
        // (only tested for TransverseMecator)
        MathTransformFactory mtf = MathTransformFactory.getDefault();
        MathTransform transform2 = mtf.createParameterizedTransform( 
                                           ( (ProjectedCoordinateSystem)dest ).getProjection() );
        MathTransform finalTransform = mtf.createConcatenatedTransform( transform1, transform2 );
        return finalTransform;
    }

    /**
     * retuns a <tt>MathTransform</tt> object for the transformation of
     * coodinates from a projected to a geographic coodinate systems
     */
    private MathTransform getProjectedToGeogr( CoordinateSystem src, CoordinateSystem dest )
                                       throws Exception {
        // get transfrom of dest to EPSG:4326. inverting this
        // results in a transformation of a projected system to
        // EPSG:4326
        CoordinateSystem tmp = csFactory.getCSByName( "EPSG 4326" );
        CoordinateTransformation transformation = trFactory.createFromCoordinateSystems( dest, tmp );
        MathTransform transform1 = transformation.getMathTransform();
        // get transfrom EPSG:4326 to projected system
        MathTransformFactory mtf = MathTransformFactory.getDefault();
        MathTransform transform2 = mtf.createParameterizedTransform( 
                                           ( (ProjectedCoordinateSystem)src ).getProjection() );
        MathTransform finalTransform = mtf.createConcatenatedTransform( transform1, transform2 );
        finalTransform = finalTransform.inverse();
        return finalTransform;
    }

    /**
     * The program main entry point. Only for educational use.
     *
     * @param  args Array of command-line arguments (there are none!).
     *
    public static void main (String [] args) {
        TransformationFactory fac = new TransformationFactory();
    }
    */
}