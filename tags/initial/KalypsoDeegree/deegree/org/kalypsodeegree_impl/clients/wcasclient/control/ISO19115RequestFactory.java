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
Lesser General public static License for more details.

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
package org.deegree_impl.clients.wcasclient.control;

import java.io.StringReader;

import org.deegree.enterprise.control.RPCMember;
import org.deegree.enterprise.control.RPCStruct;
import org.deegree.gml.GMLBox;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.wfs.filterencoding.Operation;
import org.deegree.xml.XMLTools;
import org.deegree_impl.clients.wcasclient.CatalogClientException;
import org.deegree_impl.clients.wcasclient.Constants;
import org.deegree_impl.clients.wcasclient.configuration.CSWClientConfiguration;
import org.deegree_impl.gml.GMLBox_Impl;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.model.ct.GeoTransformer;
import org.deegree_impl.services.wfs.filterencoding.Literal;
import org.deegree_impl.services.wfs.filterencoding.OperationDefines;
import org.deegree_impl.services.wfs.filterencoding.PropertyIsCOMPOperation;
import org.deegree_impl.services.wfs.filterencoding.PropertyIsLikeOperation;
import org.deegree_impl.services.wfs.filterencoding.PropertyIsNullOperation;
import org.deegree_impl.services.wfs.filterencoding.PropertyName;
import org.deegree_impl.services.wfs.filterencoding.SpatialOperation;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;
import org.w3c.dom.Document;


/**
 * class for creating a get GetRecord Request against a catalog based on OGC
 * Stateless Web Service Catalog Profil and GDI NRW catalog specifications to
 * access data metadata (ISO 19115).<p>
 * The only public method of the class receives a 'model' represented by a
 * <tt>HashMap</tt> that contains the request parameters as name-vailue-pairs.
 * The names corresponds to the formular-field-names. For common this will be
 * the fields of a HTML-form but it can be any other form (e.g. swing-application)
 * </p>
 * <p> --------------------------------------------------------------------</p>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 12.3.2003
 */
public class ISO19115RequestFactory {
    private static final char WILDCARD = '*';
    private static RPCStruct struct = null;
    private static CSWClientConfiguration conf = CSWClientConfiguration.getInstance();

    /**
     * creates a GetRecord request that is conform to the OGC Stateless Web
     * Service Catalog Profil and GDI NRW catalog specifications from a RPC struct.     
     *
     * @param struct RPC structure containing the request parameter
     * @return GetFeature request as a string
     */
    public static String createRequest( RPCStruct struct ) throws CatalogClientException {
        Debug.debugMethodBegin( "ISO19115RequestFactory", "createRequest(RPCStruct)" );

        ISO19115RequestFactory.struct = struct;
        
        // first step
        String filter = createFilterEncoding();

        if ( filter != null ) {
            // second step
            Debug.debugMethodEnd();
            return buildRequest( filter );
        } else {
            Debug.debugMethodEnd();
            throw new CatalogClientException( "filter creation fails" );
        }
    }

    /**
     * takes RequestModel and builds a String result out of it.
     * The result should be OGC FilterEncoding conformant.
     */
    private static String createFilterEncoding() throws CatalogClientException {
        //Debug.level = Debug.ALL;
        Debug.debugMethodBegin( "ISO19115RequestFactory", "createFilterEncoding()" );

        StringBuffer sb = new StringBuffer( 2000 );
        int expCounter = 0;

        sb.append( "<ogc:Filter>" );
        
        // filter out just those datasets that are marked as valid in the client
        // configuration file
        String[] filterIDs = conf.getFilterIDs();
        if ( filterIDs.length > 0 ) {
            sb.append( "<ogc:Or>" );
            for (int i = 0; i < filterIDs.length; i++) {
                expCounter++;
                Operation op = createOperation( OperationDefines.PROPERTYISLIKE, 
                                                "OBJ_ID", filterIDs[i] );
                sb.append( op.toXML() );
            }
            sb.append( "</ogc:Or>" );
        }

        // build filter encoding structure, handle all known fields sequentially              
        String s = handleFileIdentifier();
        if ( ( s != null ) && ( s.length() > 0 ) ) {
            expCounter++;
            sb.append( s );
        }
        
        s = handleParentIdentifier();
        if ( ( s != null ) && ( s.length() > 0 ) ) {
            expCounter++;
            sb.append( s );
        }
        
        s = handleFreeSearch();
        if ( ( s != null ) && ( s.length() > 0 ) ) {
            expCounter++;
            sb.append( s );
        }
        
        s = handleTopiccategory();        
        if ( ( s != null ) && ( s.length() > 0 ) ) {
            expCounter++;
            sb.append( s );
        }
        
        s = handleKeywords();
        if ( ( s != null ) && ( s.length() > 0 ) ) {
            expCounter++;
            sb.append( s );
        }
        
        s = handleDate();
        if ( ( s != null ) && ( s.length() > 0 ) ) {
            expCounter++;
            sb.append( s );
        }
        
        s = handleBbox();
        if ( ( s != null ) && ( s.length() > 0 ) ) {
            expCounter++;
            sb.append( s );
        }

        if ( expCounter > 1 ) {
            sb.insert( "<ogc:Filter>".length(), "<ogc:And>" );
            sb.append( "</ogc:And>" );
        }

        sb.append( "</ogc:Filter>" );

        Debug.debugMethodEnd();

        return sb.toString();
    }

    /**
     * Build OGC Filterencoding fragment:
     * use <code>WCASRequestmodel</code> field <b>fileIdentifier</b> to create 
     * Comparison Operation.
     */
    private static String handleFileIdentifier() throws CatalogClientException {
        Debug.debugMethodBegin( "ISO19115RequestFactory", "handleFileIdentifier" );

        StringBuffer sb = new StringBuffer(1000);        
        
        String id = null;
        if ( struct.getMember( Constants.RPC_FILEIDENTIFIER ) != null ) {
            id = (String)struct.getMember( Constants.RPC_FILEIDENTIFIER ).getValue();
        }
                
        if ( ( id != null ) && ( id.trim().length() > 0 ) ) {
            String[] cf = conf.getCatalogElements( Constants.CONF_FILEIDENTIFIER );
            sb = new StringBuffer(1000);
            Operation op1 = createOperation( OperationDefines.PROPERTYISEQUALTO, cf[0], id );
            sb.append( op1.toXML(  ) );
        }

        Debug.debugMethodEnd();
        return sb.toString();
    }

    /**
     * Build OGC Filterencoding fragment:
     * use <code>WCASRequestmodel</code> field <b>parentIdentifier</b> to create 
     * Comparison Operation.
     */
    private static String handleParentIdentifier() throws CatalogClientException {
        Debug.debugMethodBegin( "ISO19115RequestFactory", "handleParentIdentifier" );

        StringBuffer sb = new StringBuffer(1000);  
        String id = null;
        if ( struct.getMember( Constants.RPC_DATASERIES ) != null ) {
            id = (String)struct.getMember( Constants.RPC_DATASERIES ).getValue();        
        }

        if ( ( id != null ) && ( id.trim().length() > 0 ) ) {
            String[] cf = conf.getCatalogElements( Constants.CONF_DATASERIES );
            sb = new StringBuffer(1000);
            Operation op1 = createOperation( OperationDefines.PROPERTYISEQUALTO, cf[0], id );
            sb.append( op1.toXML( ) );
        }

        Debug.debugMethodEnd();
        return sb.toString();
    }

    /**
     * Spread <code>WCASRequestmodel</code> field <b>terms</b> to several Comparison 
     * Operations with pre-defined Property names.
     */
    private static String handleFreeSearch() throws CatalogClientException {
        Debug.debugMethodBegin( "ISO19115RequestFactory", "handleFreeSearch" );

        StringBuffer sb = new StringBuffer( 2000 );
        
        String[] t = null;
        if ( struct.getMember( Constants.RPC_FREESEARCH ) != null ) {
            String s = (String)struct.getMember( Constants.RPC_FREESEARCH ).getValue();
            t = StringExtend.toArray( s, ",;|", true );                
        }
        
        if ( ( t != null ) && ( t.length > 0 ) ) {
            sb.append( "<ogc:Or>" );

            for ( int i = 0; i < t.length; i++ ) {
                // replace invalid chars
                if ( ( t[i] != null ) && ( t[i].length() > 0 ) ) {
                    t[i] = StringExtend.replace( t[i], "'", " ", true );
                    t[i] = StringExtend.replace( t[i], "\"", " ", true );

                    // determine the way to build FilterEncoding part
                    String[] cf = conf.getCatalogElements( Constants.CONF_FREESEARCH );

                    for ( int k = 0; k < cf.length; k++ ) {
                        String strOp = t[i];

                        if ( ( strOp != null ) && ( strOp.length() > 0 ) ) {
                            // LOWERCASE SECTION
                            strOp = strOp.substring( 0, 1 ).toLowerCase() + strOp.substring( 1 );

                            Operation op = 
                                createOperation( OperationDefines.PROPERTYISLIKE, cf[k], strOp );
                            sb.append( op.toXML() );

                            // FIRST LETTER UPPERCASE SECTION
                            strOp = strOp.substring( 0, 1 ).toUpperCase() + strOp.substring( 1 );
                            op = createOperation( OperationDefines.PROPERTYISLIKE, cf[k], strOp );
                            sb.append( op.toXML( )  );
                        }
                    }
                }
            }

            sb.append( "</ogc:Or>" );
        }

        Debug.debugMethodEnd();
        return sb.toString();
    }

    /**
     * Build OGC Filterencoding fragment:
     * for <code>WCASRequestmodel</code> field <b>topiccategory</b>.
     */
    private static String handleTopiccategory() throws CatalogClientException {
        Debug.debugMethodBegin( "ISO19115RequestFactory", "handleTopiccategory" );

        String tc = null;
        if ( struct.getMember( Constants.RPC_TOPICCATEGORY ) != null ) {
            tc = (String)struct.getMember( Constants.RPC_TOPICCATEGORY ).getValue();
        }

        if ( tc != null  && !tc.startsWith( "..." ) && tc.length() > 0 ) {
            String[] cf = conf.getCatalogElements( Constants.CONF_TOPICCATEGORY );

            Operation op1 = createOperation( OperationDefines.PROPERTYISEQUALTO, cf[0], tc );
            tc = op1.toXML( ).toString();            
        } else {
            tc = null;
        }

        Debug.debugMethodEnd();
        return tc;
    }

    /**
     * Build OGC Filterencoding fragment:
     * Spread <code>WCASRequestmodel</code> field <b>keywords</b> to one 
     * Comparison Operations.
     */
    private static String handleKeywords() throws CatalogClientException {
        Debug.debugMethodBegin( "ISO19115RequestFactory", "handleKeywords" );

        StringBuffer sb = new StringBuffer(1000);        
        String[] tc = null;
        if ( struct.getMember( Constants.RPC_KEYWORDS ) != null ) {
            String s = (String)struct.getMember( Constants.RPC_KEYWORDS ).getValue();
            tc = StringExtend.toArray( s, ",;", true );
        }

        if ( ( tc != null ) && ( tc.length > 0 ) ) {
            String[] cf = conf.getCatalogElements( Constants.CONF_KEYWORDS );
            sb = new StringBuffer( 1000 );
            int i = 0;

            for ( i = 0; i < tc.length; i++ ) {
                if ( tc[i].trim().length() > 0 ) {
                    Operation op1 = 
                        createOperation( OperationDefines.PROPERTYISEQUALTO, cf[0], tc[i] );
                    sb.append( op1.toXML(  ) );
                }
            }

            if ( i > 1 ) {
                sb.insert( 0, "<ogc:Or>" );
                sb.append( "</ogc:Or>" );
            }
        }

        Debug.debugMethodEnd();
        return sb.toString();
    }

    /**
     * Build OGC Filterencoding fragment:
     * use <code>WCASRequestmodel</code> fields <b>dateFrom</b> to create 
     * Comparison Operation.
     */
    private static String handleDate() throws CatalogClientException {
        Debug.debugMethodBegin( "ISO19115RequestFactory", "handleDateFrom" );

        RPCMember fMem = struct.getMember( Constants.RPC_DATEFROM );
        RPCMember tMem = struct.getMember( Constants.RPC_DATETO );
        String fy = null;
        String fm = null;
        String fd = null;
        if ( fMem != null ) {
            RPCStruct st = (RPCStruct)fMem.getValue();
            if ( st.getMember( Constants.RPC_YEAR ) != null ) {
                fy = st.getMember( Constants.RPC_YEAR ).getValue().toString();
            }
            if ( st.getMember( Constants.RPC_MONTH ) != null ) {
                fm = st.getMember( Constants.RPC_MONTH ).getValue().toString();
            }                
            if ( st.getMember( Constants.RPC_DAY ) != null ) {       
                fd = st.getMember( Constants.RPC_DAY ).getValue().toString();
            }
        }
        String ty = null;
        String tm = null;
        String td = null;
        if ( tMem != null ) {
            RPCStruct st = (RPCStruct)tMem.getValue();
            if ( st.getMember( Constants.RPC_YEAR ) != null ) {
                ty = st.getMember( Constants.RPC_YEAR ).getValue().toString();
            }
            if ( st.getMember( Constants.RPC_MONTH ) != null ) {
                tm = st.getMember( Constants.RPC_MONTH ).getValue().toString();
            }                
            if ( st.getMember( Constants.RPC_DAY ) != null ) {
                td = st.getMember( Constants.RPC_DAY ).getValue().toString();
            }
        }

        
        if ( fy == null ) {
            fy = "0000";
        }

        if ( fm == null ) {
            fm = "1";
        }

        if ( Integer.parseInt( fm ) < 10 ) {
            fm = "0" + fm;
        }

        if ( fd == null ) {
            fd = "1";
        }

        if ( Integer.parseInt( fd ) < 10 ) {
            fd = "0" + fd;
        }

        String df = fy + fm + fd;
        String[] cf = CSWClientConfiguration.getInstance().getCatalogElements( Constants.CONF_DATEFROM );
        
        if ( ty == null ) {
            ty = "9999";
        }

        if ( tm == null ) {
            tm = "1";
        }

        if ( Integer.parseInt( tm ) < 10 ) {
            tm = "0" + tm;
        }

        if ( td == null ) {
            td = "1";
        }

        if ( Integer.parseInt( td ) < 10 ) {
            td = "0" + td;
        }

        String dt = ty + tm + td;
        String[] ct = conf.getCatalogElements( Constants.CONF_DATETO );

        String s = null;

        if ( ( ty != null ) && ( ty.length() > 0 ) ) {
            StringBuffer sb = new StringBuffer( "<ogc:Or>" );
            Operation op1 = null;
            sb.append( "<ogc:And>" );
            op1 = createOperation( OperationDefines.PROPERTYISGREATERTHANOREQUALTO, cf[0], df );
            sb.append( op1.toXML(  ) );
            op1 = createOperation( OperationDefines.PROPERTYISLESSTHANOREQUALTO, cf[0], dt );
            sb.append( op1.toXML( ) );
            sb.append( "</ogc:And>" );

            sb.append( "<ogc:And>" );
            op1 = createOperation( OperationDefines.PROPERTYISGREATERTHANOREQUALTO, ct[0], df );
            sb.append( op1.toXML( ) );
            op1 = createOperation( OperationDefines.PROPERTYISLESSTHANOREQUALTO, ct[0], dt );
            sb.append( op1.toXML( ) );
            sb.append( "</ogc:And>" );

            sb.append( "</ogc:Or>" );
            s = sb.toString();
        }

        Debug.debugMethodEnd();
        return s;
    }

    /**
     * Build OGC Filterencoding fragment:
     * use <code>WCASRequestmodel</code> field <b>geographicBox</b> to create Comparison Operation.
     */
    private static String handleBbox() throws CatalogClientException {
        Debug.debugMethodBegin( );

        StringBuffer sb = new StringBuffer( 1000 );        
        if ( struct.getMember( Constants.RPC_BBOX ) != null ) {
            RPCStruct bboxStruct = (RPCStruct)struct.getMember( Constants.RPC_BBOX ).getValue();

            Double minx = (Double)bboxStruct.getMember( Constants.RPC_BBOXMINX ).getValue();
            Double miny = (Double)bboxStruct.getMember( Constants.RPC_BBOXMINY ).getValue();
            Double maxx = (Double)bboxStruct.getMember( Constants.RPC_BBOXMAXX ).getValue();
            Double maxy = (Double)bboxStruct.getMember( Constants.RPC_BBOXMAXY ).getValue();

            GM_Envelope bbox = GeometryFactory.createGM_Envelope( minx.doubleValue(), miny.doubleValue(),
            													  maxx.doubleValue(), maxy.doubleValue() );       
            try {
                // transform request boundingbox to EPSG:4326 because a ISO 19115
                // compliant catalog must store the bbox of an entry like this
                GeoTransformer gt = new GeoTransformer( "EPSG:4326" );
                String srs = CSWClientConfiguration.getInstance().getWMSClientConfiguration().getInitialGetMapRequest().getSrs();
                bbox = gt.transformEnvelope( bbox, srs );
            } catch(Exception e) {
                throw new CatalogClientException( e.toString(), e );
            }
            
            Document doc = null;

            try {
                doc = XMLTools.parse( new StringReader( createGMLBox( bbox ) ) );
            } catch ( Exception e ) {
                e.printStackTrace();
                throw new CatalogClientException( e.toString() );
            }

            GMLBox box = new GMLBox_Impl( doc.getDocumentElement() );

            String[] cf = conf.getCatalogElements( Constants.CONF_GEOGRAPHICBOX );

            if ( box != null ) {
                Operation op1 = createOperation( OperationDefines.BBOX, cf[0], box );
                sb.append( op1.toXML( ) );   
            }
        }

        Debug.debugMethodEnd();
        return sb.toString();
    }

    /**
     *
     *
     * @param bbox bounding box to be used as filter condition
     *
     * @return 
     */
    private static String createGMLBox( GM_Envelope bbox ) {
        StringBuffer sb = new StringBuffer( 1000 );

        sb.append( "<gml:Box xmlns:gml=\"http://www.opengis.net/gml\" >" );
        sb.append( "<gml:coord><gml:X>" );
        sb.append( "" + bbox.getMin().getX() );
        sb.append( "</gml:X><gml:Y>" );
        sb.append( "" + bbox.getMin().getY() );
        sb.append( "</gml:Y></gml:coord><gml:coord><gml:X>" );
        sb.append( "" + bbox.getMax().getX() );
        sb.append( "</gml:X><gml:Y>" );
        sb.append( "" + bbox.getMax().getY() );
        sb.append( "</gml:Y></gml:coord></gml:Box>" );

        return sb.toString();
    }

    /**
     *
     */
    private static Operation createOperation( int opId, String prop, Object value ) {
        Debug.debugMethodBegin( "ISO19115RequestFactory", "createOperation" );

        Operation op = null;

        switch ( opId ) {
            case OperationDefines.PROPERTYISEQUALTO:
                op = new PropertyIsCOMPOperation( OperationDefines.PROPERTYISEQUALTO, 
                                                  new PropertyName( prop ), 
                                                  new Literal( (String)value ) );
                break;
            case OperationDefines.PROPERTYISLIKE:

                char wildCard = WILDCARD;
                char singleChar = '?';
                char escapeChar = '/';
                String lit = wildCard + (String)value + wildCard;
                op = new PropertyIsLikeOperation( new PropertyName( prop ), new Literal( lit ), 
                                                  wildCard, singleChar, escapeChar );
                break;
            case OperationDefines.PROPERTYISLESSTHANOREQUALTO:
                op = new PropertyIsCOMPOperation( OperationDefines.PROPERTYISLESSTHANOREQUALTO, 
                                                  new PropertyName( prop ), 
                                                  new Literal( (String)value ) );
                break;
            case OperationDefines.PROPERTYISGREATERTHANOREQUALTO:
                op = new PropertyIsCOMPOperation( OperationDefines.PROPERTYISGREATERTHANOREQUALTO, 
                                                  new PropertyName( prop ), 
                                                  new Literal( (String)value ) );
                break;
            case OperationDefines.BBOX:
                op = new SpatialOperation( OperationDefines.BBOX, new PropertyName( prop ), 
                                           (GMLBox)value );
                break;
            case OperationDefines.PROPERTYISNULL:
                op = new PropertyIsNullOperation( new PropertyName( prop ) );
                break;
            default:
                op = new PropertyIsCOMPOperation( OperationDefines.PROPERTYISEQUALTO, 
                                                  new PropertyName( prop ), 
                                                  new Literal( (String)value ) );
        }

        Debug.debugMethodEnd();
        return op;
    }

    /**
     * second step to create Request: take filter and build request
     */
    private static String buildRequest( String filter ) throws CatalogClientException {
        Debug.debugMethodBegin( "ISO19115RequestFactory", "buildRequest( ... )" );

        StringBuffer req = new StringBuffer( 5000 );

        req.append( "<?xml version='1.0' encoding='UTF-8'?>" );
        req.append( "<GetRecord " );
        req.append( " xmlns:wfs=\"http://www.opengis.net/wfs\"" );
        req.append( " xmlns:ogc=\"http://www.opengis.net/ogc\"" );
        req.append( " xmlns:gml=\"http://www.opengis.net/gml\"" );

        
        if ( struct.getMember( Constants.RPC_MAXRECORDS ) != null ) {
            req.append( " maxRecords='" + struct.getMember( Constants.RPC_MAXRECORDS ).getValue() + "'" );
        } else {
            req.append( " maxRecords='-1'" );
        }

        if ( struct.getMember( Constants.RPC_OUTPUTFORMAT ) != null ) {
            req.append( " outputFormat='" + struct.getMember( Constants.RPC_OUTPUTFORMAT ).getValue() + "'" );
        } else {
            req.append( " outputFormat=\"XML\"" );
        }

        if ( struct.getMember( Constants.RPC_OUTPUTRECTYPE ) != null ) {
            req.append( " outputRecType='" + struct.getMember( Constants.RPC_OUTPUTRECTYPE ).getValue() + "'" );
        } else {
            req.append( " outputRecType=\"ISO19115\"" );
        }

        if ( struct.getMember( Constants.RPC_QUERYSCOPE ) != null ) {
            req.append( " queryScope='" + struct.getMember( Constants.RPC_QUERYSCOPE ) + "'" );
        } else {
            req.append( " queryScope='" + conf.getQueryScope() + "'" );
        }

        if ( struct.getMember( Constants.RPC_STARTPOSITION ) != null ) {
            req.append( " startPosition='" +struct.getMember( Constants.RPC_STARTPOSITION ) + "'>" );
        } else {
            req.append( " startPosition='0'>" );
        }

        req.append( createQuery( filter ) );
        req.append( "</GetRecord>" );

        Debug.debugMethodEnd();

        return req.toString();
    }

    /**
     * create Query expression using input params
     * @filter: holds FilterEncoding statemen
     * @tn: holds typeName value, if no value available get one using the model-accessor
     */
    private static String createQuery( String filter ) {
        Debug.debugMethodBegin( "ISO19115RequestFactory", "createQuery( ... )" );

        StringBuffer q = new StringBuffer();
        String sn = "Brief";
        if ( struct.getMember( Constants.RPC_SETNAME ) != null ) {
            sn = (String)struct.getMember( Constants.RPC_SETNAME ).getValue();
        }

        String typeName = "Product";
        if ( struct.getMember( Constants.RPC_TYPENAME )  != null ) {
            typeName = (String)struct.getMember( Constants.RPC_TYPENAME ).getValue();
        }
        if ( typeName.toString().equalsIgnoreCase( Constants.PRODUCT ) ) {
            q.append( "<Query typeName=\"" + Constants.PRODUCT + "\">" );
        } else if ( typeName.equalsIgnoreCase( Constants.COLLECTION ) ) {
            q.append( "<Query typeName=\"" + Constants.COLLECTION + "\">" );
        } else if (typeName.equalsIgnoreCase( Constants.SERVICE ) ) {
            q.append( "<Query typeName=\"" + Constants.SERVICE + "\">" );
        }

        q.append( "<PropertySet setName=\"" + sn + "\"/>" );
        q.append( filter );
        q.append( "</Query>" );

        Debug.debugMethodEnd();

        return q.toString();
    }
}