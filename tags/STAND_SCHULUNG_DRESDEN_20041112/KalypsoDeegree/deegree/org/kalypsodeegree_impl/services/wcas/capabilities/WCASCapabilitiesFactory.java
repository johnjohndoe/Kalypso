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
package org.deegree_impl.services.wcas.capabilities;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.capabilities.CException;
import org.deegree.services.capabilities.DCPType;
import org.deegree.services.capabilities.MetadataURL;
import org.deegree.services.wcas.capabilities.Capability;
import org.deegree.services.wcas.capabilities.FederatedCatalog;
import org.deegree.services.wcas.capabilities.GetCapabilities;
import org.deegree.services.wcas.capabilities.Operation;
import org.deegree.services.wcas.capabilities.PresentOptions;
import org.deegree.services.wcas.capabilities.QueryLanguages;
import org.deegree.services.wcas.capabilities.RecordType;
import org.deegree.services.wcas.capabilities.RecordTypeList;
import org.deegree.services.wcas.capabilities.Request;
import org.deegree.services.wcas.capabilities.TaxonomyType;
import org.deegree.services.wcas.capabilities.TaxonomyTypeList;
import org.deegree.services.wcas.capabilities.WCASCapabilities;
import org.deegree.services.wcas.metadatadesc.ISO19119;
import org.deegree.xml.XMLTools;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.services.capabilities.CException_Impl;
import org.deegree_impl.services.capabilities.MetadataURL_Impl;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * Factory class for creating a catalog capabilities object from a XML document.
 * 
 * <p>
 * ---------------------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
public final class WCASCapabilitiesFactory
{

  private static String namespace = null;

  private static String iso19119ns = null;

  private static String iso19115ns = null;

  /**
   * factory method for creating a <tt>WCASCapabilities</tt> object from a
   * file that contains XML capabilities document
   */
  public static synchronized WCASCapabilities createCapabilities( URL url ) throws IOException,
      SAXException, Exception
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "createCapabilities" );

    InputStreamReader isr = new InputStreamReader( url.openStream() );
    Document doc = XMLTools.parse( isr );
    WCASCapabilities capabilities = createCapabilities( doc );

    Debug.debugMethodEnd();

    return capabilities;
  }

  /**
   * factory method for creating a <tt>WFSCapabilities</tt> object from a OGC
   * WFS 1.0.0 conform XML capabilities document
   */
  public static synchronized WCASCapabilities createCapabilities( Document doc ) throws Exception
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "createCapabilities" );

    Element root = doc.getDocumentElement();
    namespace = root.getNamespaceURI();

    //get general service informations
    String version = XMLTools.getAttrValue( root, "version" );
    String updateSequence = XMLTools.getAttrValue( root, "updateSequence" );
    iso19119ns = XMLTools.getAttrValue( root, "xmlns:iso19119" );
    iso19115ns = XMLTools.getAttrValue( root, "xmlns:iso19115_full" );

    // get service section
    Element element = (Element)root.getElementsByTagNameNS( namespace, "ISO19119" ).item( 0 );
    ISO19119 iso19119 = getISO19119( element );

    // get capability section
    element = (Element)root.getElementsByTagNameNS( namespace, "Capability" ).item( 0 );
    Capability capability = getCapability( element );

    // get locations of the catalogs schema definitions
    NodeList nl = root.getElementsByTagNameNS( namespace, "SchemaLocations" );
    HashMap schemaLocations = getSchemaLocations( nl );

    // create capabilities object
    WCASCapabilities capabilities = new WCASCapabilities_Impl( version, updateSequence, iso19119,
        capability, schemaLocations );

    Debug.debugMethodEnd();

    return capabilities;
  }

  /**
   * returns the locations of the schema definitions that describes the reponses
   * of catalog
   */
  private static HashMap getSchemaLocations( NodeList nl ) throws Exception
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getSchemaLocations" );

    HashMap locations = new HashMap();

    for( int i = 0; i < nl.getLength(); i++ )
    {
      String format = XMLTools.getAttrValue( nl.item( i ), "format" );
      String s = XMLTools.getAttrValue( nl.item( i ), "href" );
      locations.put( format, new URL( s ) );
    }

    Debug.debugMethodEnd();
    return locations;
  }

  /**
   * returns the metadata description of the catalog
   */
  private static ISO19119 getISO19119( Element element )
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getISO19119" );

    Debug.debugMethodEnd();
    return null;
  }

  /**
   * returns the addresses and protocols a request can be reached
   */
  private static DCPType[] getDCPTypes( NodeList nl )
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getDCPTypes" );
    Debug.debugMethodEnd();
    return null;
  }

  /**
   * returns an instance of an object that capsulates the service element of the
   * WFS capabilities.
   */
  private static Capability getCapability( Element capElement ) throws Exception
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getCapability" );

    // get capability request element
    Element element = XMLTools.getNamedChild( capElement, namespace, "TaxonomyTypeList" );
    TaxonomyTypeList taxonomyTypeList = null;
    if( element != null )
    {
      getTaxonomyTypeList( element );
    }

    // get capability request element
    element = XMLTools.getNamedChild( capElement, namespace, "Request" );
    Request request = null;
    if( element != null )
    {
      getRequest( element );
    }

    // get the record type list
    element = XMLTools.getNamedChild( capElement, namespace, "RecordTypeList" );
    RecordTypeList recordTypeList = null;
    if( element != null )
    {
      getRecordTypeList( element );
    }

    // get the present options
    element = XMLTools.getNamedChild( capElement, namespace, "PresentOptions" );
    PresentOptions presentOptions = null;
    if( element != null )
    {
      getPresentOptions( element );
    }

    // get all known federated catalogs
    NodeList nl = capElement.getElementsByTagNameNS( namespace, "QueryLanguage" );
    QueryLanguages queryLanguages = getQueryLanguages( nl );

    // get all known federated catalogs
    nl = capElement.getElementsByTagNameNS( namespace, "FederatedCatalogType" );
    FederatedCatalog[] federatedCatalogs = getFederatedCatalogs( nl );

    // get the exception format
    element = XMLTools.getNamedChild( capElement, namespace, "Exceptions" );
    CException exceptions = null;
    if( element != null )
    {
      getExceptions( element );
    }

    // get capability vendor specific capabilities
    element = XMLTools.getNamedChild( capElement, namespace, "VendorSpecificCapabilities" );
    Document vendor = null;
    if( element != null )
    {
      getVendorSpecificCapabilities( element );
    }

    Capability capability = new Capability_Impl( taxonomyTypeList, request, recordTypeList,
        presentOptions, queryLanguages, federatedCatalogs, exceptions, vendor );

    Debug.debugMethodEnd();
    return capability;
  }

  /**
   * returns the query languages known by the catalog
   */
  private static QueryLanguages getQueryLanguages( NodeList nl ) throws Exception
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getQueryLanguages" );

    String[] languages = new String[nl.getLength()];

    for( int i = 0; i < languages.length; i++ )
    {
      languages[i] = XMLTools.getAttrValue( nl.item( i ), "name" );
    }

    QueryLanguages queryLanguages = new QueryLanguages_Impl( languages );

    Debug.debugMethodEnd();
    return queryLanguages;
  }

  /**
   * returns the description of taxonomies contained within the capabilities
   */
  private static TaxonomyTypeList getTaxonomyTypeList( Element element ) throws Exception
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getTaxonomyTypeList" );

    NodeList nl = element.getElementsByTagNameNS( namespace, "TaxonomyType" );

    TaxonomyType[] tt = new TaxonomyType[nl.getLength()];

    for( int i = 0; i < nl.getLength(); i++ )
    {
      Element elem = (Element)nl.item( i );
      // name of the taxonomy
      String name = XMLTools.getNamedChild( elem, namespace, "Name" ).getFirstChild()
          .getNodeValue();
      // title of the taxonomy
      Node node = XMLTools.getNamedChild( elem, namespace, "Title" );
      String title = null;
      if( node != null )
      {
        title = node.getFirstChild().getNodeValue();
      }
      // abstract description of the taxonomy
      node = XMLTools.getNamedChild( elem, iso19119ns, "abstract" );
      String abstract_ = null;
      if( node != null )
      {
        abstract_ = node.getFirstChild().getNodeValue();
      }
      // language used within the taxonomy
      node = XMLTools.getNamedChild( elem, namespace, "languageCode" );
      String languageCode = null;
      if( node != null )
      {
        languageCode = node.getFirstChild().getNodeValue();
      }
      // get URL of the XML document describing the taxonomy
      Element el = (Element)elem.getElementsByTagNameNS( iso19119ns, "linkage" ).item( 0 );
      String s = XMLTools.getAttrValue( el, "href" );
      URL url = new URL( s );

      tt[i] = new TaxonomyType_Impl( name, title, abstract_, languageCode, url );
    }

    TaxonomyTypeList taxonomyTypeList = new TaxonomyTypeList_Impl( tt );

    Debug.debugMethodEnd();

    return taxonomyTypeList;
  }

  /**
   * returns the request description contained within the capabilities
   */
  private static Request getRequest( Element element )
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getRequest" );

    // get capability request element
    Element elem = XMLTools.getNamedChild( element, namespace, "GetCapabilities" );
    GetCapabilities getCapabilities = getGetCapabilities( elem );

    // get capability request element
    elem = XMLTools.getNamedChild( element, namespace, "DescribeRecordType" );

    // get capability request element
    elem = XMLTools.getNamedChild( element, namespace, "GetRecord" );

    // get capability request element
    elem = XMLTools.getNamedChild( element, namespace, "LockRecord" );

    // get capability request element
    elem = XMLTools.getNamedChild( element, namespace, "Transaction" );

    // get capability request element
    elem = XMLTools.getNamedChild( element, namespace, "RegisterService" );

    Debug.debugMethodEnd();
    return null;
  }

  /**
   * returns the GetCapabilities description
   */
  private static GetCapabilities getGetCapabilities( Element element )
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getGetCapabilities" );

    NodeList nl = element.getElementsByTagNameNS( iso19119ns, "DCPType" );
    DCPType[] dcpTypes = getDCPTypes( nl );

    GetCapabilities getCapabilities = new GetCapabilities_Impl( dcpTypes );

    Debug.debugMethodEnd();
    return getCapabilities;
  }

  /**
   * returns the GetRecord description
   */
  //    private static GetRecord getGetRecord(Element element )
  //    {
  //        Debug.debugMethodBegin();
  //        
  //        NodeList nl = element.getElementsByTagNameNS( iso19119ns, "DCPType" );
  //        DCPType[] dcpTypes = getDCPTypes( nl );
  //        
  //        Element elem = XMLTools.getNamedChild( element, namespace, "ResultFormat"
  // );
  //        String[] resultFormats = getResultFormats( elem );
  //        
  //        GetRecord getRecord = new GetRecord_Impl( dcpTypes, resultFormats, null );
  //        
  //        Debug.debugMethodEnd();
  //        return getRecord;
  //    }
  /**
   * returns the LockRecord description
   */
  //    private static LockRecord getLockRecord(Element element )
  //    {
  //        Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getLockRecord");
  //        
  //        NodeList nl = element.getElementsByTagNameNS( iso19119ns, "DCPType" );
  //        DCPType[] dcpTypes = getDCPTypes( nl );
  //                
  //        LockRecord lockRecord = new LockRecord_Impl( dcpTypes );
  //        
  //        Debug.debugMethodEnd();
  //        return lockRecord;
  //    }
  /**
   * returns the Transaction description
   */
  //    private static Transaction getTransaction(Element element )
  //    {
  //        Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getTransaction");
  //        
  //        NodeList nl = element.getElementsByTagNameNS( iso19119ns, "DCPType" );
  //        DCPType[] dcpTypes = getDCPTypes( nl );
  //                
  //        Transaction transaction = new Transaction_Impl( dcpTypes );
  //        
  //        Debug.debugMethodEnd();
  //        return transaction;
  //    }
  /**
   * returns the RegisterService description
   */
  //    private static RegisterService getRegisterService(Element element )
  //    {
  //        Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getRegisterService");
  //        
  //        NodeList nl = element.getElementsByTagNameNS( iso19119ns, "DCPType" );
  //        DCPType[] dcpTypes = getDCPTypes( nl );
  //                
  //        RegisterService registerService = new RegisterService_Impl( dcpTypes );
  //        
  //        Debug.debugMethodEnd();
  //        return registerService;
  //    }
  /**
   * returns the formats a GetRecord request can serve
   */
  //    private static String[] getResultFormats(Element element )
  //    {
  //        Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getResultFormats");
  //        
  //        ArrayList list = new ArrayList();
  //        NodeList nl = element.getChildNodes();
  //        
  //        for (int i = 0; i < nl.getLength(); i++) {
  //            if ( nl.item(i) instanceof Element ) {
  //                list.add( nl.item(i).getLocalName() );
  //            }
  //        }
  //        
  //        String[] tmp = (String[])list.toArray( new String[list.size()] );
  //        
  //        Debug.debugMethodEnd();
  //        return tmp;
  //    }
  /**
   * returns the record type list description contained within the capabilities
   */
  private static RecordTypeList getRecordTypeList( Element element ) throws Exception
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getRecordTypeList" );

    NodeList nl = element.getElementsByTagNameNS( namespace, "RecordType" );
    RecordType[] recordTypes = getRecordTypes( nl );

    Debug.debugMethodEnd();
    return null;
  }

  /**
   * returns the record type available through the catalog
   */
  private static RecordType[] getRecordTypes( NodeList nl ) throws Exception
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getRecordTypes" );

    RecordType[] recordTypes = new RecordType[nl.getLength()];

    for( int i = 0; i < nl.getLength(); i++ )
    {
      Element elem = (Element)nl.item( i );
      // get name of the record type
      String name = XMLTools.getNamedChild( elem, namespace, "Name" ).getFirstChild()
          .getNodeValue();
      // get title of the record type
      Node node = XMLTools.getNamedChild( elem, iso19119ns, "title" );
      String title = null;
      if( node != null )
      {
        title = node.getFirstChild().getNodeValue();
      }
      // get abstract description of the record type
      node = XMLTools.getNamedChild( elem, iso19119ns, "abstract" );
      String abstract_ = null;
      if( node != null )
      {
        abstract_ = node.getFirstChild().getNodeValue();
      }
      // get get keywords associated with the record type
      String[] keywords = null;
      NodeList nl_ = elem.getElementsByTagNameNS( iso19119ns, "keywords" );
      if( nl_ != null && nl_.getLength() > 0 )
      {
        keywords = getKeywords( nl_ );
      }
      // get srs (crs) of the record type
      node = XMLTools.getNamedChild( elem, namespace, "SRS" );
      String crs = null;
      if( node != null )
      {
        crs = node.getFirstChild().getNodeValue();
      }
      // get bounding box of ????
      GM_Envelope bb = null;
      Element el = (Element)elem.getElementsByTagNameNS( iso19115ns, "LatLonBoundingBox" ).item( 0 );
      if( el != null )
      {
        bb = getLatLonBoundingBox( el );
      }
      // get operations that can be performed on the record type
      el = (Element)elem.getElementsByTagNameNS( namespace, "Operations" ).item( 0 );
      Operation[] operations = getOperations( el );
      // get metadata description of the record type
      el = (Element)elem.getElementsByTagNameNS( namespace, "MetadataDesc" ).item( 0 );
      MetadataURL[] metadataURLs = getMetadataURLs( el );

      RecordType rt = new RecordType_Impl( name, title, abstract_, crs, bb, null, keywords,
          operations, metadataURLs, null, null );
      recordTypes[i] = rt;
    }

    Debug.debugMethodEnd();
    return recordTypes;
  }

  /**
   * returns the keywords of a RecordType
   */
  private static String[] getKeywords( NodeList nl )
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getKeywords" );
    Debug.debugMethodEnd();
    return null;
  }

  /**
   * returns the bounding box of a RecordType
   */
  private static GM_Envelope getLatLonBoundingBox( Element element )
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getLatLonBoundingBox" );

    Element el = XMLTools.getNamedChild( element, iso19115ns, "westBoundLongitude" );
    double minx = Double.parseDouble( el.getFirstChild().getNodeValue() );
    el = XMLTools.getNamedChild( element, iso19115ns, "eastBoundLongitude" );
    double maxx = Double.parseDouble( el.getFirstChild().getNodeValue() );
    el = XMLTools.getNamedChild( element, iso19115ns, "southBoundLongitude" );
    double miny = Double.parseDouble( el.getFirstChild().getNodeValue() );
    el = XMLTools.getNamedChild( element, iso19115ns, "northBoundLongitude" );
    double maxy = Double.parseDouble( el.getFirstChild().getNodeValue() );

    GM_Envelope bb = GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );

    Debug.debugMethodEnd();
    return bb;
  }

  /**
   * returns the operations of the record types available through the catalog
   */
  private static Operation[] getOperations( Element element )
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getRecordType" );

    ArrayList list = new ArrayList();

    NodeList nl = element.getChildNodes();
    for( int i = 0; i < nl.getLength(); i++ )
    {
      if( nl.item( i ) instanceof Element )
      {
        list.add( new Operation_Impl( nl.item( i ).getLocalName() ) );
      }
    }

    Operation[] ops = (Operation[])list.toArray( new Operation[list.size()] );

    Debug.debugMethodEnd();
    return ops;
  }

  /**
   * returns the metadata descriptions of a RecordType
   */
  private static MetadataURL[] getMetadataURLs( Element element ) throws Exception
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getMetadataURLs" );

    String format = XMLTools.getAttrValue( element, "schema" );

    NodeList nl = element.getElementsByTagNameNS( namespace, "ElementSet" );
    NodeList nl_ = element.getElementsByTagNameNS( iso19119ns, "MetadataURL" );
    MetadataURL[] metadataURL = new MetadataURL[nl.getLength()];

    for( int i = 0; i < nl.getLength(); i++ )
    {
      String type = XMLTools.getAttrValue( nl.item( i ), "type" );
      String href = XMLTools.getAttrValue( nl_.item( i ), "href" );
      URL url = new URL( href );
      metadataURL[i] = new MetadataURL_Impl( type, format, url );
    }

    Debug.debugMethodEnd();
    return metadataURL;
  }

  /**
   * returns the present options description contained within the capabilities
   */
  private static PresentOptions getPresentOptions( Element element )
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getPresentOptions" );

    String s = XMLTools.getAttrValue( element, "StartRec" );
    boolean startRec = ( s == null || s.equals( "true" ) || s.equals( "1" ) );
    s = XMLTools.getAttrValue( element, "Hits" );
    boolean hits = ( s == null || s.equals( "true" ) || s.equals( "1" ) );
    s = XMLTools.getAttrValue( element, "RecsMax" );
    int recsMax = Integer.MAX_VALUE;
    if( s != null )
    {
      recsMax = Integer.parseInt( s );
    }
    PresentOptions po = new PresentOptions_Impl( startRec, hits, recsMax );

    Debug.debugMethodEnd();
    return po;
  }

  /**
   * returns the federated catalogs that are known by the catalog
   */
  private static FederatedCatalog[] getFederatedCatalogs( NodeList nl ) throws Exception
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getFederatedCatalogs" );

    FederatedCatalog[] federatedCatalogs = new FederatedCatalog[nl.getLength()];

    for( int i = 0; i < nl.getLength(); i++ )
    {
      Element el = (Element)nl.item( i );
      // get name of the record type
      String name = XMLTools.getNamedChild( el, namespace, "Name" ).getFirstChild().getNodeValue();
      // get title of the record type
      Node node = XMLTools.getNamedChild( el, iso19119ns, "title" );
      String title = null;
      if( node != null )
      {
        title = node.getFirstChild().getNodeValue();
      }
      // get abstract description of the record type
      node = XMLTools.getNamedChild( el, iso19119ns, "abstract" );
      String abstract_ = null;
      if( node != null )
      {
        abstract_ = node.getFirstChild().getNodeValue();
      }
      Element elem = (Element)el.getElementsByTagNameNS( iso19119ns, "CatalogURL" ).item( 0 );
      String href = XMLTools.getAttrValue( elem, "href" );
      URL url = new URL( href );

      federatedCatalogs[i] = new FederatedCatalog_Impl( name, title, abstract_, url );
    }

    Debug.debugMethodEnd();
    return federatedCatalogs;
  }

  /**
   * returns the format exception will be retruned
   */
  private static CException getExceptions( Element element )
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getExceptions" );

    Node node = XMLTools.getNamedChild( element, namespace, "Format" );
    String s = null;
    if( node != null )
    {
      s = node.getFirstChild().getNodeValue();
    }

    CException exception = new CException_Impl( new String[]
    { s } );

    Debug.debugMethodEnd();
    return exception;
  }

  /**
   * returns vendor specific capabilities
   */
  private static Document getVendorSpecificCapabilities( Element element )
  {
    Debug.debugMethodBegin( "WCASCapabilitiesFactory", "getRequest" );
    Debug.debugMethodEnd();
    return null;
  }

}