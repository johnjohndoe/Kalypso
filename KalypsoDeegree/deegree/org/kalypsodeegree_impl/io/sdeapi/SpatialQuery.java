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
package org.deegree_impl.io.sdeapi;

import com.esri.sde.sdk.client.*;

import java.io.ByteArrayInputStream;

import java.util.ArrayList;
import java.util.Vector;

import org.deegree.model.geometry.*;
import org.deegree.model.table.Table;
import org.deegree.model.table.TableException;

import org.deegree_impl.model.geometry.*;
import org.deegree_impl.model.table.Table_Impl;
import org.deegree_impl.tools.Debug;


/**
 * This class handles a complete ArcSDE request:
 * If instanciated, the class can open a connection/instance of the specified
 * ArcSDE server, set a bounding box as a spatial filter to query the defined
 * layer. The resultset of the query contains the geometries as well as the
 * tabular data associated with them. The table is stored as a deegree Table
 * object whereas the geometries are stored as an array of deegree GM_Objects.
 * Depending on the datatype of the geometries, the array of GM_Objects might
 * be GM_Point, GM_Curve etc.
 * <p>
 * Some bits of sample code to create a query:
 * <p>
 * <code>
 *        SpatialQuery sq = new SpatialQuery();<br>
 *        try {<br>
 *        &nbsp;sq.openConnection(server, instance, database, user, password);<br>
 *        &nbsp;sq.setLayer(layer);<br>
 *        &nbsp;sq.setSpatialFilter(minX, minY, maxX, maxY);<br>
 *        &nbsp;sp.runSpatialQuery();<br>
 *        &nbsp;GM_Object[] deegree_gm_obj = sq.getGeometries();<br>
 *        &nbsp;Table deegree_table = sq.getTable();<br>
 *        &nbsp;sq.closeConnection();<br>
 *        } catch ( SeException sexp ) {<br>
 *        &nbsp;System.out.println("SeException : " + sexp.getSeError().getErrDesc());<br>
 *        }<br>
 * </code>
 * @author <a href="mailto:bedel@giub.uni-bonn.de">Markus Bedel</a>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class SpatialQuery {
    // Connection to SDE
    private SeConnection conn = null;
    // Currently opened Layer and associated Table
    private SeLayer layer = null;
    // Current Spatial Filter - a BoundingBox
    private SeShape spatialFilter = null;
    private SeTable table = null;
    // The Query ResultObjects
    private GM_Object[] deegreeGM_Objects = null;      
    

    /**
     * Creates a new SpatialQuery object.
     *
     * @param server 
     * @param port 
     * @param database 
     * @param user 
     * @param password 
     *
     * @throws SeException 
     */
    public SpatialQuery( String server, int port, String database, String user, String password )
                 throws SeException {
        openConnection( server, port, database, user, password );
    }

    /**
     * Connect to the ArcSDE server
     * <br>throws SeException
     */
    public void openConnection( String server, int port, String database, String user, 
                                String password ) throws SeException {
        Debug.debugMethodBegin( this, "openConnection" );
        conn = new SeConnection( server, port, database, user, password );
        Debug.debugMethodEnd();
    }

    /**
     * Close the current connection to the ArcSDE server
     * <br>throws SeException
     */
    public void closeConnection() throws SeException {
        conn.close();
    }

    /**
     * Set a SDE layer to work on and appropriate table
     * <br>throws SeException
     */
    public void setLayer( String layername ) throws SeException {
        Vector layerList = conn.getLayers();
        String spatialCol = null;

        for ( int i = 0; i < layerList.size(); i++ ) {
            SeLayer layer = (SeLayer)layerList.elementAt( i );
            if ( layer.getQualifiedName().trim().equalsIgnoreCase( layername ) ) {                 
                spatialCol = layer.getSpatialColumn();
                break;
            }
        }

        layer = new SeLayer( conn, layername, spatialCol );
        table = new SeTable( conn, layer.getQualifiedName() ); 
        
    }

    /**
     * Get the current SDE layer
     * <br>returns null if it not yet set.
     */
    public SeLayer getLayer() {
        return layer;
    }

    /**
     * Set a SpatialFilter to Query (BoundingBox)
     * <br>throws SeException
     */
    public void setSpatialFilter( double minx, double miny, double maxx, double maxy )
                          throws SeException {        
                
        GM_Envelope layerBBox = 
        	GeometryFactory.createGM_Envelope( layer.getExtent().getMinX(),
                                               layer.getExtent().getMinY(),
                                               layer.getExtent().getMaxX(),
                                               layer.getExtent().getMaxY() );                
        
        GM_Envelope query = GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );
        query = query.createIntersection( layerBBox );

        if ( query != null ) {
            spatialFilter = new SeShape( layer.getCoordRef() );
            SeExtent extent = new SeExtent( query.getMin().getX(), query.getMin().getY(), 
                                            query.getMax().getX(), query.getMax().getY() );
            spatialFilter.generateRectangle( extent );
        } else {
            spatialFilter = null;
        }

    }    

    /**
     * Get the current Spatial Filter
     * <br>returns null if it not yet set.
     */
    public SeShape getSpatialFilter() {
        return spatialFilter;
    }

    /**
     * Get GM_Object[] containing the queried Geometries
     * <br>returns null if no query has been done yet.
     */
    public GM_Object[] getGeometries() {
        return deegreeGM_Objects;
    }

    /**
     * Runs a spatial query against the opened layer using the specified spatial filter.
     * <br>throws SeException
     */
    public Table runSpatialQuery(String[] cols) throws SeException {
        Debug.debugMethodBegin();
                
        Table deegreeTable = null;
        if ( spatialFilter != null ) {
            SeShapeFilter[] filters = new SeShapeFilter[1];
         
            filters[0] = new SeShapeFilter( layer.getQualifiedName(), layer.getSpatialColumn(), 
                                        spatialFilter, SeFilter.METHOD_ENVP ); 
            
            SeColumnDefinition[] tableDef = table.describe();
            if ( cols == null || cols.length == 0 ) {
                cols = new String[tableDef.length];
                for ( int i = 0; i < tableDef.length; i++ ) {
                    cols[i] = tableDef[i].getName();
                }
            }

            SeSqlConstruct sqlCons = new SeSqlConstruct( layer.getQualifiedName() ); 
            SeQuery spatialQuery = new SeQuery( conn, cols, sqlCons );
          
            spatialQuery.prepareQuery();
            spatialQuery.setSpatialConstraints( SeQuery.SE_OPTIMIZE, false, filters );
            spatialQuery.execute();

            SeRow row = spatialQuery.fetch();
            
            int numRows = 0;
            if ( row != null ) {
                int numCols = row.getNumColumns();
                // Fetch all the features that satisfied the query
                deegreeTable = initTable( row );

                ArrayList list = new ArrayList( 20000 );
                Object[] tableObj = null;

                while ( row != null ) {
                    int colNum = 0;
                    tableObj = new Object[deegreeTable.getColumnCount()];
                    
                    for ( int i = 0; i < numCols; i++ ) {
                        SeColumnDefinition colDef = row.getColumnDef( i );

                        if ( row.getIndicator( (short)i ) != SeRow.SE_IS_NULL_VALUE ) {
                            switch ( colDef.getType() ) {
                                case SeColumnDefinition.TYPE_SMALLINT:
                                    tableObj[colNum++] = row.getShort( i );                                    
                                    break;
                                case SeColumnDefinition.TYPE_INTEGER:
                                    tableObj[colNum++] = row.getInteger( i );
                                    break;
                                case SeColumnDefinition.TYPE_FLOAT:
                                    tableObj[colNum++] = row.getFloat( i );
                                    break;
                                case SeColumnDefinition.TYPE_DOUBLE:
                                    tableObj[colNum++] = row.getDouble( i );
                                    break;
                                case SeColumnDefinition.TYPE_STRING:
                                    tableObj[colNum++] = row.getString( i );
                                    break;
                                case SeColumnDefinition.TYPE_BLOB:
                                    ByteArrayInputStream bis = (ByteArrayInputStream)row.getObject( i );
                                    tableObj[colNum++] = bis;
                                    break;
                                case SeColumnDefinition.TYPE_DATE:
                                    tableObj[colNum++] = row.getDate( i );
                                    break;
                                case SeColumnDefinition.TYPE_RASTER:
                                    System.out.println( "\t" + colDef.getName() + " : Cant handle this" );
                                    break;
                                case SeColumnDefinition.TYPE_SHAPE:
                                    SeShape spVal = row.getShape( i );
                                    createGeometry( spVal, list );
                                    break;
                                default:
                                    System.out.println( "Unknown Table DataType" );
                                    break;
                            } // End switch(type)
                        } // End if
                    } // End for

                    numRows++;

                    try {
                        deegreeTable.appendRow( tableObj );
                    } catch ( TableException tex ) {
                        throw new SeWarningException( tex.toString() );
                    }

                    row = spatialQuery.fetch();
                } // End while
                spatialQuery.close();

                deegreeGM_Objects = new GM_Object[list.size()];
                deegreeGM_Objects = (GM_Object[])list.toArray( deegreeGM_Objects );
            } else {
                try {
                    deegreeTable = 
                        new Table_Impl( layer.getQualifiedName(), new String[] {"NONE"}, 
                                        new String[] {"java.lang.String"}, 2 );
                } catch (Exception e) {
                    e.printStackTrace();
                }
                deegreeGM_Objects = new GM_Object[0];
            }                     
//System.out.println( "Number of returned Features: " + numRows );
        } else {
            try {
                deegreeTable = 
                    new Table_Impl( layer.getQualifiedName(), new String[] {"NONE"}, 
                                    new String[] {"java.lang.String"}, 2 );
            } catch (Exception e) {
                e.printStackTrace();
            }
            deegreeGM_Objects = new GM_Object[0];
//System.out.println( "Number of returned Features: " + 0 );
        }
        
        Debug.debugMethodEnd();
        return deegreeTable;
    } // End method runSpatialQuery

    /**
     * Initialize Table object - used with first row of the SpatialQuery
     * This method sets the TableName, TableColumnNames and their DataTypes
     * <br>throws SeException
     */
    private Table initTable( SeRow row ) throws SeException {
        Debug.debugMethodBegin();
        ArrayList colNames = new ArrayList( 50 );
        ArrayList colTypes = new ArrayList( 50 );
        Table deegreeTable = null;
        SeColumnDefinition colDef = null;

        for ( int i = 0; i < row.getNumColumns(); i++ ) {
            try {
                colDef = row.getColumnDef( i );
            } catch ( SeException sexp ) {
                sexp.printStackTrace();
                throw new SeWarningException( sexp.toString() );
            }

            switch ( colDef.getType() ) {
                case SeColumnDefinition.TYPE_SMALLINT:
                    colNames.add( colDef.getName().toUpperCase() );
                    colTypes.add( "java.lang.Short" );
                    break;
                case SeColumnDefinition.TYPE_INTEGER:
                    colNames.add( colDef.getName().toUpperCase() );
                    colTypes.add( "java.lang.Integer" );
                    break;
                case SeColumnDefinition.TYPE_FLOAT:
                    colNames.add( colDef.getName().toUpperCase() );
                    colTypes.add( "java.lang.Float" );
                    break;
                case SeColumnDefinition.TYPE_DOUBLE:
                    colNames.add( colDef.getName().toUpperCase() );
                    colTypes.add( "java.lang.Double" );
                    break;
                case SeColumnDefinition.TYPE_STRING:
                    colNames.add( colDef.getName().toUpperCase() );
                    colTypes.add( "java.lang.String" );
                    break;
                case SeColumnDefinition.TYPE_BLOB:
                    // there is an open issue with fetching blobs,
                    // look at this document:
                    // "ArcSDE 8.1 Java API - BLOB columns"
                    // http://support.esri.com/Search/KbDocument.asp?dbid=17068
                    colNames.add( colDef.getName().toUpperCase() );
                    colTypes.add( "java.lang.ByteArrayInputStream" );
                    break;
                case SeColumnDefinition.TYPE_DATE:
                    colNames.add( colDef.getName().toUpperCase() );
                    colTypes.add( "java.util.Date" );
                    break;
                default:
                    break;
            }
        }

        String[] colN = new String[colNames.size()];
        colN = (String[])colNames.toArray( colN );

        String[] colT = new String[colTypes.size()];
        colT = (String[])colTypes.toArray( colT );

        try {
            deegreeTable = new Table_Impl( layer.getQualifiedName(), colN, colT, 20000 ); 
        } catch ( TableException tex ) {
            tex.printStackTrace();
            throw new SeWarningException( tex.toString() );
        }
        Debug.debugMethodEnd();
        return deegreeTable;
    } // End Method initTable

    /**
     * CreateGeometry - used with every row of the SpatialQuery
     * Depending on the layers' geometries datatype different operations
     * are made to create the appropriate object.
     *<br>Available ArcSDE ShapeTypes:
     *<br>    TYPE_POINT (impl)
     *<br>    TYPE_MULTI_POINT (impl)
     *<br>    TYPE_SIMPLE_LINE (impl)
     *<br>    TYPE_MULTI_SIMPLE_LINE (impl)
     *<br>    TYPE_LINE (impl)
     *<br>    TYPE_MULTI_LINE (impl)
     *<br>    TYPE_POLYGON (impl)
     *<br>    TYPE_MULTI_POLYGON (impl)
     *<br>    TYPE_NIL (impl)
     *
     *<br>throws SeException
     */
    private void createGeometry( SeShape shape, ArrayList list ) throws SeException {
        
        int shptype = shape.getType();

        ArrayList al = shape.getAllPoints( SeShape.TURN_DEFAULT, true );
        // Retrieve the array of SDEPoints
        SDEPoint[] points = (SDEPoint[])al.get( 0 );
        // Retrieve the part offsets array.
        int[] partOffset = (int[])al.get( 1 );
        // Retrieve the sub-part offsets array.
        int[] subPartOffset = (int[])al.get( 2 );

        int numPoints = shape.getNumOfPoints();

        int numParts = shape.getNumParts();

        switch ( shptype ) {
            // a single point
            case SeShape.TYPE_NIL:
                GM_Point gmPoint = GeometryFactory.createGM_Point( -9E9, -9E9, null );
                list.add( gmPoint );
                System.out.println( "Found SeShape.TYPE_NIL." );
                System.out.println( "\tThe queried layer does not have valid geometries" );
                break;
            // a single point
            case SeShape.TYPE_POINT:
                gmPoint = GeometryFactory.createGM_Point( points[0].getX(), points[0].getY(), null );
                list.add( gmPoint );
                break;
            // an array of points
            case SeShape.TYPE_MULTI_POINT:
                GM_Point[] gmPoints = new GM_Point[numPoints];

                for ( int pt = 0; pt < numPoints; pt++ ) {
                    gmPoints[pt] = 
                    	GeometryFactory.createGM_Point( points[pt].getX(), points[pt].getY(), null );
                }

                try {
                    GM_MultiPoint gmMultiPoint = GeometryFactory.createGM_MultiPoint( gmPoints );
                    list.add( gmMultiPoint );
                } catch ( GM_Exception gme ) {
                    System.out.println( "Error creating GM_MultiPoint " + gme.toString() );
                    throw new SeWarningException( gme.toString() );
                }

                break;
            // a single line, simple as it does not intersect itself
            case SeShape.TYPE_SIMPLE_LINE:
            // or a single, non-simple line
            case SeShape.TYPE_LINE:

                GM_Position[] gmSimpleLinePosition = new GM_Position[numPoints];

                for ( int pt = 0; pt < numPoints; pt++ ) {
                    gmSimpleLinePosition[pt] = GeometryFactory.createGM_Position( points[pt].getX(), 
                                                                          points[pt].getY() );
                }

                try {
                    GM_Curve gmCurve = GeometryFactory.createGM_Curve( gmSimpleLinePosition, null );
                    list.add( gmCurve );
                } catch ( GM_Exception gme ) {
                    gme.printStackTrace();
                    throw new SeWarningException( gme.toString() );
                }

                break;
            // an array of lines, simple as they do not intersect with themself
            case SeShape.TYPE_MULTI_SIMPLE_LINE:
            // or an array of non-simple lines
            case SeShape.TYPE_MULTI_LINE:

                GM_Curve[] gmCurves = new GM_Curve[numParts];

                for ( int partNo = 0; partNo < numParts; partNo++ ) {
                    int lastPoint = shape.getNumPoints( partNo + 1, 1 ) + partOffset[partNo];
                    GM_Position[] gmMultiSimpleLinePosition = new GM_Position[shape.getNumPoints( partNo + 1, 1 )];
                    int i = 0;

                    for ( int pt = partOffset[partNo]; pt < lastPoint; pt++ ) {
                        gmMultiSimpleLinePosition[i] = GeometryFactory.createGM_Position( points[pt].getX(), 
                                                                                  points[pt].getY() );
                        i++;
                    }

                    try {
                        gmCurves[partNo] = GeometryFactory.createGM_Curve( gmMultiSimpleLinePosition, null );
                    } catch ( GM_Exception gme ) {
                        gme.printStackTrace();
                        throw new SeWarningException( gme.toString() );
                    }
                }

                try {
                    GM_MultiCurve gmMultiCurve = GeometryFactory.createGM_MultiCurve( gmCurves );
                    list.add( gmMultiCurve );
                } catch ( GM_Exception gme ) {
                    gme.printStackTrace();
                    throw new SeWarningException( gme.toString() );
                }

                break;
            // a single polygon which might contain islands
            case SeShape.TYPE_POLYGON:

                int numSubParts = shape.getNumSubParts( 1 );
                GM_Position[] gmPolygonExteriorRing = new GM_Position[shape.getNumPoints( 1, 1 )];

                for ( int pt = 0; pt < shape.getNumPoints( 1, 1 ); pt++ ) {
                    gmPolygonExteriorRing[pt] = GeometryFactory.createGM_Position( points[pt].getX(), 
                                                                           points[pt].getY() );
                }

                GM_Position[][] gmPolygonInteriorRings = null;

                // if it is a donut create inner rings
                if ( numSubParts > 1 ) {
                    gmPolygonInteriorRings = new GM_Position[numSubParts - 1][];

                    int j = 0;

                    for ( int subPartNo = 1; subPartNo < numSubParts; subPartNo++ ) {
                        int lastPoint = shape.getNumPoints( 1, subPartNo + 1 ) + 
                                        subPartOffset[subPartNo];
                        GM_Position[] gmPolygonPosition = new GM_Position[shape.getNumPoints( 1, 
                                                                                              subPartNo + 1 )];
                        int i = 0;

                        for ( int pt = subPartOffset[subPartNo]; pt < lastPoint; pt++ ) {
                            gmPolygonPosition[i] = GeometryFactory.createGM_Position( points[pt].getX(), 
                                                                              points[pt].getY() );
                            i++;
                        }

                        gmPolygonInteriorRings[j] = gmPolygonPosition;
                        j++;
                    }
                }

                try {
                    GM_Surface gmSurface = GeometryFactory.createGM_Surface( gmPolygonExteriorRing, 
                                                                     gmPolygonInteriorRings, 
                                                                     new GM_SurfaceInterpolation_Impl(), 
                                                                     null );
                    list.add( gmSurface );
                } catch ( GM_Exception gme ) {
                    gme.printStackTrace();
                    throw new SeWarningException( gme.toString() );
                }

                break;
            // an array of polygons which might contain islands
            case SeShape.TYPE_MULTI_POLYGON:
         
                GM_Surface[] gmMultiPolygonSurface = new GM_Surface[numParts];
                boolean subParts = false;

                if ( partOffset.length < subPartOffset.length ) {
                    subParts = true;
                }
                for ( int partNo = 0, partEnd = 0; partNo < partOffset.length; partNo++ ) {
                    GM_Position[] gmMultiPolygonExteriorRing = new GM_Position[shape.getNumPoints( partNo + 1, 1 )];
                    GM_Position[][] gmMultiPolygonInteriorRings = null;
                    int nSubParts = shape.getNumSubParts( partNo + 1 );
                    if ( nSubParts > 1 ) {
                        gmMultiPolygonInteriorRings = new GM_Position[( nSubParts - 1 )][];
                    }

                    if ( ( partOffset.length - partNo ) == 1 ) {
                        partEnd = points.length; //If this is the last part, scan through to points.length
                    } else {
                        partEnd = subPartOffset[partOffset[partNo + 1]]; //Otherwise scan to the offset of next part
                    }
                    int subPartNo = partOffset[partNo];
                    int pointNo = subPartOffset[partOffset[partNo]];
                    boolean exterior = true;
                    int i = 0;
                    int subPartIndex = -1;

                    for ( ; ( pointNo < points.length ) && ( pointNo < partEnd ); pointNo++ ) {
                        if ( subParts ) {
                            if ( ( subPartNo < subPartOffset.length ) && 
                                     ( pointNo == subPartOffset[subPartNo] ) ) {
                                subPartNo++;
                                i = 0;
                            }
                        }

                        if ( exterior ) {
                            gmMultiPolygonExteriorRing[i] = 
                            	GeometryFactory.createGM_Position( points[pointNo].getX(), 
                                                                   points[pointNo].getY() );

                            i++;

                            if ( ( subPartNo < subPartOffset.length ) && 
                                     ( pointNo == ( subPartOffset[subPartNo] - 1 ) ) ) {
                                exterior = false;
                            }
                        } else {
                            // When i=0 we are starting a new subPart. I compute and 
                            // assign the size of the second dimension of gmMultiPolygonInteriorRings
                            if ( i == 0 ) {
                                subPartIndex++; //Used to address each interior ring

                                gmMultiPolygonInteriorRings[subPartIndex] = new GM_Position[subPartOffset[subPartNo] - 
                                                                            subPartOffset[subPartNo - 1]];
                            }

                            gmMultiPolygonInteriorRings[subPartIndex][i] = 
                            	GeometryFactory.createGM_Position( points[pointNo].getX(), 
                                                                   points[pointNo].getY() );
                            i++;
                        }
                    } // End for

                    try {
                        gmMultiPolygonSurface[partNo] = 
                        	GeometryFactory.createGM_Surface( gmMultiPolygonExteriorRing, 
                                                              gmMultiPolygonInteriorRings, 
                                                              new GM_SurfaceInterpolation_Impl(), 
                                                              null );
                    } catch ( GM_Exception gme ) {
                        gme.printStackTrace();
                        throw new SeWarningException( gme.toString() );
                    }
                } // End for

                try {
                    GM_MultiSurface gmMultiSurface = 
                    	GeometryFactory.createGM_MultiSurface( gmMultiPolygonSurface );
                    list.add( gmMultiSurface );
                } catch ( GM_Exception gme ) {
                    gme.printStackTrace();
                    throw new SeWarningException( gme.toString() );
                }

                break;
            default:
                System.out.println( "Unknown GeometryType - ID: " + shape.getType() );
                break;
        } // End of switch
    } // End Method createGeometry
} // End Class SpatialQueryEx
