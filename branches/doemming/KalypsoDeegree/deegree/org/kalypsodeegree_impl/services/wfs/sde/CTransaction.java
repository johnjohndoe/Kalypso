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
package org.deegree_impl.services.wfs.sde;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.HashMap;

import org.deegree.gml.GMLFeature;
import org.deegree.gml.GMLProperty;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.wfs.configuration.DatastoreConfiguration;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.protocol.WFSDelete;
import org.deegree.services.wfs.protocol.WFSInsert;
import org.deegree.services.wfs.protocol.WFSInsertResult;
import org.deegree.services.wfs.protocol.WFSOperation;
import org.deegree.services.wfs.protocol.WFSTransactionRequest;
import org.deegree.services.wfs.protocol.WFSUpdate;
import org.deegree_impl.io.sdeapi.Transaction;
import org.deegree_impl.services.OGCWebServiceException_Impl;
import org.deegree_impl.services.wfs.AbstractTransaction;
import org.deegree_impl.services.wfs.filterencoding.PointDBSQLBuilder;
import org.deegree_impl.services.wfs.protocol.WFSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;


/**
 * inner interface defining the processing of a transaction request
 */
class CTransaction extends AbstractTransaction {
    private DatastoreConfiguration config = null;

    /**
     * Creates a new CTransaction object.
     *
     * @param parent 
     * @param request 
     */
    public CTransaction( SDEDataStore parent, OGCWebServiceRequest request ) {
        super( parent, request );
        config = parent.getConfiguration();
    }

    /**
     *
     *
     * @param request 
     *
     * @return 
     */
    protected OGCWebServiceResponse[] performRequest( OGCWebServiceRequest request ) {
        Debug.debugMethodBegin();

        OGCWebServiceResponse[] response = new OGCWebServiceResponse[1];

        WFSTransactionRequest tr = (WFSTransactionRequest)request;
        String handle = tr.getHandle();
//        String lockId = tr.getLockId();
//        String releaseAction = tr.getReleaseAction();
        WFSOperation[] operations = tr.getOperations();
        String[] aft = getAffectedFeatureTypes( operations );
        ArrayList inR = new ArrayList();
        int[] oI = new int[operations.length];

        try {
            // handle operations defined by the request
            for ( int i = 0; i < operations.length; i++ ) {
                if ( operations[i] instanceof WFSInsert ) {
                    Object[] o = performInsert( (WFSInsert)operations[i] );

                    if ( o[0] != null ) {
                        inR.add( o[0] );
                    }

                    // if this value equals 0 no expection raised during
                    // insertion. if the value is positiv some insertions
                    // failed. if the value is < 0 all insertions failed
                    oI[i] = ( (Integer)o[1] ).intValue();
                } else if ( operations[i] instanceof WFSUpdate ) {
                    if (performUpdate ((WFSUpdate) operations [i])) {
                        oI[i] = 0;
                    } else {
                        oI[i] = -1;
                    }
                } else if ( operations[i] instanceof WFSDelete ) {
                    if (performDelete ((WFSDelete) operations [i])) {
                        oI[i] = 0;
                    } else {
                        oI[i] = -1;
                    }
                } else {
                    // native request
                }
            }

            String status = null;
            boolean succ = true;
            boolean failed = true;

            for ( int i = 0; i < oI.length; i++ ) {
                if ( oI[i] != 0 ) {
                    succ = false;
                }

                if ( oI[i] > 0 ) {
                    failed = false;
                }
            }

            if ( succ ) {
                status = "SUCCESS";
            } else if ( failed ) {
                status = "FAILED";
            } else {
                status = "PARTIAL";
            }

            // create response object
            WFSInsertResult[] ir = new WFSInsertResult[inR.size()];
            ir = (WFSInsertResult[])inR.toArray( ir );
            response[0] = WFSProtocolFactory.createWFSTransactionResponse( request, aft, null, ir, status, 
                                                                handle );
        } catch ( Exception e ) {
            Debug.debugException( e, null );

            OGCWebServiceException exce = new OGCWebServiceException_Impl( 
                                                  "CTransaction: performRequest", 
                                                  StringExtend.stackTraceToString( 
                                                          e.getStackTrace() ) );
            response[0] = WFSProtocolFactory.createWFSGetFeatureResponse( request, aft, exce, null );
        }

        Debug.debugMethodEnd();
        return response;
    }

    /**
     * performs a insert of a flat feature into the ArcSDE
     *
     * @param insert insert object
     *
     * @return WFSInsertResults
     *
     * @throws Exception 
     */
    private Object[] performInsert( WFSInsert insert ) throws Exception {
        Debug.debugMethodBegin();

        WFSInsertResult result = null;
        Transaction sq = null;

        // database insert attempts
        int k1 = 0;
        // successful inserts
        int k2 = 0;
        
        ArrayList idlist = null;
        String handle = "";
        try {
            String[] ftNames = insert.getFeatureTypes();
            idlist = new ArrayList();            

            for ( int i = 0; i < ftNames.length; i++ ) {
                String ftName = ftNames[i];

                if ( parent.isKnownFeatureType( ftName ) ) {
                    org.deegree.services.wfs.configuration.FeatureType featureType = 
                            config.getFeatureType( ftName );
                    GMLFeature[] gmlFeatures = insert.getFeatures( ftName );

                    // open SDE transaction connection
                    sq = openConnection();
                    sq.setLayer( featureType.getMasterTable().getName() );

                    for ( int j = 0; j < gmlFeatures.length; j++ ) {
                        idlist.add( gmlFeatures[j].getId() );
                        GMLProperty[] gmlProperties = gmlFeatures[j].getProperties();
                        HashMap row = new HashMap();
System.out.println("---- Insert Feature ----");
                        for ( int k = 0; k < gmlProperties.length; k++ ) {
                            String name = gmlProperties[k].getName().replace('.', '/');
                            String[] s = featureType.getDatastoreField( name );
                            if ( s == null || s.length == 0 ) {
                                throw new Exception( "no SDE field for property: " +
                                                     name + " could be found!" );
                            }
                            row.put( s[0], gmlProperties[k].getPropertyValue() ); 
System.out.println("prop: " + name  + "   SDE field: " + s[0] + "   value: " + gmlProperties[k].getPropertyValue());
                        }    
System.out.println("----------");
                        try {
                            k1++;
                            sq.insertFeature( row );
                            k2++;
                        } catch (Exception e) {
                            Debug.debugException( e, "" );
                            // store/collect error message(s) as handle that will be
                            // submitted to the WFSInsertResult
                            handle += ( e.getMessage() + "\n" );
                        }                        
                    }
                }
            }            
        } catch ( Exception e ) {
            throw e;
        } finally {
            sq.closeConnection();            
        }
        String[] ids = (String[])idlist.toArray( new String[idlist.size()] );
        result = WFSProtocolFactory.createWFSInsertResult( handle, ids );
        // calculate success status
        // 0 = success
        // > 0 = failed partially
        // < 0 = failed completely
        int k = k1 - k2;

        if ( k2 == 0 ) {
            k *= -1;
        }
        

        Debug.debugMethodEnd();
        return new Object[] { result, new Integer( k ) };
    }
    
    /**
     * performs an update against the ArcSDE
     *
     * @return true if every thing worked as it should
     */
    private boolean performUpdate (WFSUpdate update) throws Exception  {
        Debug.debugMethodBegin();
System.out.println("---- Update Features ----");        
        org.deegree.services.wfs.configuration.FeatureType featureType = 
                            config.getFeatureType( update.getTypeName() );
        
        HashMap map = update.getProperties();
        HashMap row = new HashMap();
        Iterator iterator = map.keySet().iterator();
        while ( iterator.hasNext() ) {
            String key = (String)iterator.next();
            String name = key.replace('.', '/');
            String[] s = featureType.getDatastoreField( name );
            if ( s == null || s.length == 0 ) {
                throw new Exception( "no SDE field for property: " +
                                     name + " could be found!" );
            }
            Object value = map.get( key );
            row.put( s[0], value );
System.out.println("prop: " + name  + "   SDE field: " + s[0] + "   value: " + value);
        }

        
        boolean suc = true;
        try {           
            // create where clause
            Filter filter = update.getFilter();        
            PointDBSQLBuilder builder = new PointDBSQLBuilder( featureType );
            String where =builder.filter2SQL( filter );
            int pos = where.indexOf( "WHERE" );
            where = where.substring( pos + "WHERE".length() );
System.out.println( "update condition: " + where);
            // open the connection to the ArcSDE to perform the update
            Transaction transaction = openConnection();
            transaction.setLayer( featureType.getMasterTable().getName() );
            transaction.updateFeature( row , where, null );
            transaction.closeConnection();
        } catch (Exception e) {
            e.printStackTrace();
            suc = false;
        }
System.out.println("-------------");  
        Debug.debugMethodEnd();
        return suc;
    }
    
    /**
     * performs a delete against the ArcSDE
     *
     * @return true if every thing worked as it should
     */
    private boolean performDelete(WFSDelete delete) {
        Debug.debugMethodBegin();
        
        boolean suc = true;
        try {
            org.deegree.services.wfs.configuration.FeatureType featureType = 
                            config.getFeatureType( delete.getTypeName() );
        
            // create where clause
            Filter filter = delete.getFilter();        
            PointDBSQLBuilder builder = new PointDBSQLBuilder( featureType );
            String where =builder.filter2SQL( filter );
            int pos = where.indexOf( "WHERE" );
            where = where.substring( pos + "WHERE".length() );
System.out.println( "delete condition: " + where);
            // open the connection to the ArcSDE to perform the delete
            Transaction transaction = openConnection();
            transaction.setLayer( featureType.getMasterTable().getName() );
            transaction.deleteFeature( where, null );
            transaction.closeConnection();
        } catch (Exception e) {
            e.printStackTrace();
            suc = false;
        }
        
        Debug.debugMethodEnd();
        return suc;
    }

    /**
     * initializes the SDEAPI Transaction class and opens an connection to the
     * current SDE instance
     *
     * @return Transaction
     *
     * @throws Exception 
     */
    private Transaction openConnection() throws Exception {
        // get server address and port
        String logon = config.getConnection().getLogon();
        int pos = logon.indexOf( ':' );
        String server = logon.substring( 0, pos );
        String ports = logon.substring( pos + 1, logon.length() );
        int port = 80;

        if ( ( ports != null ) && ( ports.length() > 0 ) ) {
            port = Integer.parseInt( ports );
        }

        String database = config.getConnection().getSDEDatabase();

        // proceed if openConnection succeeds
        Transaction sq = new Transaction( server, port, database, config.getConnection().getUser(), 
                                          config.getConnection().getPassword() );
        return sq;
    }
}