/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 This library is free software; you can reibute it and/or
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
package org.deegree_impl.clients.wcasclient.model;

import java.net.URL;
import java.util.Calendar;

import org.deegree.xml.ElementList;
import org.deegree.xml.XMLTools;
import org.deegree_impl.clients.wcasclient.CatalogClientException;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.TimeTools;
import org.w3c.dom.Element;

/**
 * 
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class MetadataFactory
{
  /**
   * creates a BaseMD_Metadata object from it DOM representation.
   * 
   * @param element
   *          DOM representation of a brief catalog response
   * 
   * @return BaseMD_Metadata
   * 
   * @throws CatalogClientException
   */
  public static BaseMetadata createISO19115Brief( Element element, String namespace )
      throws CatalogClientException
  {
    Debug.debugMethodBegin( "MetadataFactory", "createISO19115Brief" );

    BaseMetadata mdMetadata = null;

    try
    {
      String fileIdentifier = XMLTools
          .getRequiredStringValue( "fileIdentifier", namespace, element );

      String metadataStandardName = XMLTools.getRequiredStringValue( "metadataStandardName",
          namespace, element );

      String metadataStandardVersion = XMLTools.getRequiredStringValue( "metadataStandardVersion",
          namespace, element );

      Calendar dateStamp = null;
      String dateSt = XMLTools.getStringValue( "dateStamp", namespace, element, null );
      if( dateSt != null && dateSt.length() > 3 )
      {
        dateStamp = TimeTools.createCalendar( dateSt );
      }

      // get title
      ElementList el = XMLTools.getChildElementsByName( "identificationInfo", namespace, element );
      el = XMLTools.getChildElementsByName( "MD_DataIdentification", namespace, el.item( 0 ) );
      el = XMLTools.getChildElementsByName( "citation", namespace, el.item( 0 ) );
      String title = XMLTools.getRequiredStringValue( "title", namespace, el.item( 0 ) );

      // create a MD_Metadata entry from the parameter extracted from the
      // ISO19115 brief XML structure
      mdMetadata = new BaseMetadata( title, fileIdentifier, dateStamp, metadataStandardName,
          metadataStandardVersion );
    }
    catch( Exception e )
    {
      throw new CatalogClientException( e.toString() );
    }

    Debug.debugMethodEnd();
    return mdMetadata;
  }

  /**
   * creates a DetailedMD_Metadata object from it DOM representation.
   * 
   * @param element
   *          DOM representation of a full catalog response
   * 
   * @return DetailedMD_Metadata
   * 
   * @throws CatalogClientException
   */
  public static DetailedMetadata createISO19115Full( Element element, String namespace )
      throws CatalogClientException
  {
    Debug.debugMethodBegin();

    DetailedMetadata mdMetadata = null;
    try
    {
      String fileIdentifier = XMLTools
          .getRequiredStringValue( "fileIdentifier", namespace, element );

      String metadataStandardName = XMLTools.getRequiredStringValue( "metadataStandardName",
          namespace, element );

      String metadataStandardVersion = XMLTools.getRequiredStringValue( "metadataStandardVersion",
          namespace, element );

      Calendar dateStamp = null;
      String dateSt = XMLTools.getStringValue( "dateStamp", namespace, element, null );
      if( dateSt != null )
      {
        dateStamp = TimeTools.createCalendar( dateSt );
      }

      // get title and abstract
      ElementList el = XMLTools.getChildElementsByName( "identificationInfo", namespace, element );
      el = XMLTools.getChildElementsByName( "MD_DataIdentification", namespace, el.item( 0 ) );
      String abstract_ = XMLTools.getStringValue( "abstract", namespace, el.item( 0 ), null );
      el = XMLTools.getChildElementsByName( "citation", namespace, el.item( 0 ) );
      String title = XMLTools.getRequiredStringValue( "title", namespace, el.item( 0 ) );

      // get resource constraints
      el = XMLTools.getChildElementsByName( "identificationInfo", namespace, element );
      el = XMLTools.getChildElementsByName( "MD_DataIdentification", namespace, el.item( 0 ) );
      el = XMLTools.getChildElementsByName( "resourceConstraints", namespace, el.item( 0 ) );
      String constraints = null;
      if( el.getLength() > 0 )
      {
        constraints = getResourceConstraints( namespace, el.item( 0 ) );
      }

      // get spatial resolution
      el = XMLTools.getChildElementsByName( "identificationInfo", namespace, element );
      el = XMLTools.getChildElementsByName( "MD_DataIdentification", namespace, el.item( 0 ) );
      el = XMLTools.getChildElementsByName( "spatialResolution", namespace, el.item( 0 ) );
      String equivalentScale = null;
      if( el.getLength() > 0 )
      {
        equivalentScale = getEquivalentScale( namespace, el.item( 0 ) );
      }

      // get keywords
      el = XMLTools.getChildElementsByName( "identificationInfo", namespace, element );
      el = XMLTools.getChildElementsByName( "MD_DataIdentification", namespace, el.item( 0 ) );
      el = XMLTools.getChildElementsByName( "descriptiveKeywords", namespace, el.item( 0 ) );
      String[] keywords = null;
      if( el.getLength() > 0 )
      {
        keywords = getDescriptiveKeywords( namespace, el.item( 0 ) );
      }

      el = XMLTools.getChildElementsByName( "identificationInfo", namespace, element );
      el = XMLTools.getChildElementsByName( "MD_DataIdentification", namespace, el.item( 0 ) );
      el = XMLTools.getChildElementsByName( "extent", namespace, el.item( 0 ) );
      // time extent
      Calendar[] tex = getTimeExtent( namespace, el.item( 0 ) );
      // get boundingbox
      //GM_Envelope[] bboxes = getGeographicBoundingBox( namespace, el.item(0)
      // );

      //get contact
      el = XMLTools.getChildElementsByName( "contact", namespace, element );
      if( el.getLength() == 0 )
      {
        throw new CatalogClientException( "at least one contact must be defined in"
            + " a metadataset" );
      }
      Contact[] contacts = getContact( namespace, el );

      // get spatial referennce system
      String crs = null;
      el = XMLTools.getChildElementsByName( "referenceSystemInfo", namespace, element );
      if( el.getLength() > 0 )
      {
        crs = getCRS( namespace, el.item( 0 ) );
      }

      // gets the optional lineageSourceDescription and LineageStatement
      String lineageSrcDesc = null;
      String lineageStatement = null;
      String processStep = null;
      el = XMLTools.getChildElementsByName( "dataQualityInfo", namespace, element );
      if( el.getLength() > 0 )
      {
        lineageSrcDesc = getLineageSrcDescription( namespace, el.item( 0 ) );
        lineageStatement = getLineageStatement( namespace, el.item( 0 ) );
        processStep = getProcessStep( namespace, el.item( 0 ) );
      }

      // gets the optional maintenanceAndUpdateFrequency
      String maintenanceAndUpdateFrequency = null;
      el = XMLTools.getChildElementsByName( "identificationInfo", namespace, element );
      el = XMLTools.getChildElementsByName( "MD_DataIdentification", namespace, el.item( 0 ) );
      el = XMLTools.getChildElementsByName( "resourceMaintenance", namespace, el.item( 0 ) );
      if( el.getLength() > 0 )
      {
        maintenanceAndUpdateFrequency = getMaintenanceAndUpdateFrequency( namespace, el.item( 0 ) );
      }

      OnlineTransferOption[] onlineTransferOptions = null;
      el = XMLTools.getChildElementsByName( "distributionInfo", namespace, element );
      if( el.getLength() > 0 )
        el = XMLTools.getChildElementsByName( "MD_Distribution", namespace, el.item( 0 ) );
      if( el.getLength() > 0 )
        el = XMLTools.getChildElementsByName( "transferOptions", namespace, el.item( 0 ) );
      if( el.getLength() > 0 )
        el = XMLTools.getChildElementsByName( "MD_DigitalTransferOptions", namespace, el.item( 0 ) );
      if( el.getLength() > 0 )
        el = XMLTools.getChildElementsByName( "online", namespace, el.item( 0 ) );
      if( el.getLength() > 0 )
        onlineTransferOptions = getOnlineTransferOptions( namespace, el );

      mdMetadata = new DetailedMetadata( title, fileIdentifier, dateStamp, metadataStandardName,
          metadataStandardVersion, tex[0], tex[1], abstract_, crs, lineageSrcDesc,
          lineageStatement, maintenanceAndUpdateFrequency, constraints, processStep,
          equivalentScale, keywords, contacts, onlineTransferOptions );
    }
    catch( Exception e )
    {
      Debug.debugException( e, "" );
      throw new CatalogClientException( e.toString(), e );
    }

    Debug.debugMethodEnd();
    return mdMetadata;
  }

  /**
   * returns the list of contacts for a Metadataset
   */
  private static Contact[] getContact( String namespace, ElementList el )
      throws CatalogClientException
  {
    Debug.debugMethodBegin();

    Contact[] contacts = new Contact[el.getLength()];
    try
    {
      for( int i = 0; i < el.getLength(); i++ )
      {
        String name = XMLTools.getStringValue( "individualName", namespace, el.item( i ), "-" );
        String organisation = XMLTools.getStringValue( "organisationName", namespace, el.item( i ),
            "-" );
        String role = XMLTools.getStringValue( "role", namespace, el.item( i ), "-" );
        // get address
        ElementList ell = XMLTools.getChildElementsByName( "contactInfo", namespace, el.item( i ) );
        ell = XMLTools.getChildElementsByName( "address", namespace, ell.item( 0 ) );
        String street = XMLTools.getStringValue( "deliveryPoint", namespace, ell.item( 0 ), "-" );
        String postalCode = XMLTools.getStringValue( "postalCode", namespace, ell.item( 0 ), "-" );
        String city = XMLTools.getStringValue( "city", namespace, ell.item( 0 ), "-" );
        String country = XMLTools.getStringValue( "country", namespace, ell.item( 0 ), "-" );
        String email = XMLTools.getStringValue( "electronicMailAddress", namespace, ell.item( 0 ),
            "-" );
        // get phone
        ell = XMLTools.getChildElementsByName( "contactInfo", namespace, el.item( i ) );
        ell = XMLTools.getChildElementsByName( "phone", namespace, ell.item( 0 ) );
        String phone = XMLTools.getStringValue( "voice", namespace, ell.item( 0 ), "-" );
        String fax = XMLTools.getStringValue( "facsimile", namespace, ell.item( 0 ), "-" );
        // get Online Resource
        ell = XMLTools.getChildElementsByName( "contactInfo", namespace, el.item( i ) );
        ell = XMLTools.getChildElementsByName( "onlineResource", namespace, ell.item( 0 ) );
        String onlineResource = XMLTools.getStringValue( "linkage", namespace, ell.item( 0 ), "-" );

        // create contact information
        contacts[i] = new Contact( name, fax, phone, street, city, postalCode, country, email,
            role, organisation, onlineResource );
      }
    }
    catch( Exception e )
    {
      throw new CatalogClientException( e.toString(), e );
    }

    Debug.debugMethodEnd();
    return contacts;
  }

  /**
   * returns the begin and end
   */
  private static Calendar[] getTimeExtent( String namespace, Element element )
      throws CatalogClientException
  {
    Debug.debugMethodBegin();

    Calendar[] tex = null;
    try
    {
      ElementList el = XMLTools.getChildElementsByName( "temporalElement", namespace, element );
      el = XMLTools.getChildElementsByName( "EX_TemporalExtent", namespace, el.item( 0 ) );
      el = XMLTools.getChildElementsByName( "extent", namespace, el.item( 0 ) );
      el = XMLTools.getChildElementsByName( "beginEnd", namespace, el.item( 0 ) );
      if( el != null && el.getLength() > 0 )
      {
        tex = new Calendar[2];
        String begin = XMLTools.getStringValue( "begin", "http://www.isotc211.org/iso19115/", el
            .item( 0 ), null );
        String end = XMLTools.getStringValue( "end", "http://www.isotc211.org/iso19115/", el
            .item( 0 ), null );
        if( begin != null )
        {
          tex[0] = TimeTools.createCalendar( begin );
        }
        if( end != null )
        {
          tex[1] = TimeTools.createCalendar( end );
        }
      }
    }
    catch( Exception e )
    {
      throw new CatalogClientException( e.toString(), e );
    }

    Debug.debugMethodEnd();
    return tex;
  }

  /**
   * returns the reference system identifier
   */
  private static String getCRS( String namespace, Element element ) throws CatalogClientException
  {
    Debug.debugMethodBegin();

    String code = null;
    try
    {
      ElementList el = XMLTools.getChildElementsByName( "MD_ReferenceSystem", namespace, element );
      el = XMLTools.getChildElementsByName( "referenceSystemIdentifier", namespace, el.item( 0 ) );
      code = XMLTools.getRequiredStringValue( "code", namespace, el.item( 0 ) );
    }
    catch( Exception e )
    {
      throw new CatalogClientException( e.toString(), e );
    }

    Debug.debugMethodEnd();
    return code;
  }

  /**
   * returns the optional lineageSourceDescription value
   */
  private static String getLineageSrcDescription( String namespace, Element element )
      throws CatalogClientException
  {
    Debug.debugMethodBegin();

    String lineageSrcDesc = null;
    try
    {
      ElementList el = XMLTools.getChildElementsByName( "DQ_DataQuality", namespace, element );
      el = XMLTools.getChildElementsByName( "lineage", namespace, el.item( 0 ) );
      if( el.getLength() > 0 )
      {
        el = XMLTools.getChildElementsByName( "LI_Lineage", namespace, el.item( 0 ) );
        el = XMLTools.getChildElementsByName( "source", namespace, el.item( 0 ) );
        if( el.getLength() > 0 )
        {
          el = XMLTools.getChildElementsByName( "LI_Source", namespace, el.item( 0 ) );
          lineageSrcDesc = XMLTools.getStringValue( "description", namespace, el.item( 0 ), null );
        }
      }
    }
    catch( Exception e )
    {
      throw new CatalogClientException( e.toString(), e );
    }

    Debug.debugMethodEnd();
    return lineageSrcDesc;
  }

  /**
   * returns the optional lineageStatement value
   */
  private static String getLineageStatement( String namespace, Element element )
      throws CatalogClientException
  {
    Debug.debugMethodBegin();

    String lineageStatement = null;
    try
    {
      ElementList el = XMLTools.getChildElementsByName( "DQ_DataQuality", namespace, element );
      el = XMLTools.getChildElementsByName( "lineage", namespace, el.item( 0 ) );
      if( el.getLength() > 0 )
      {
        el = XMLTools.getChildElementsByName( "LI_Lineage", namespace, el.item( 0 ) );
        lineageStatement = XMLTools.getStringValue( "statement", namespace, el.item( 0 ), null );

      }
    }
    catch( Exception e )
    {
      throw new CatalogClientException( e.toString(), e );
    }

    Debug.debugMethodEnd();
    return lineageStatement;
  }

  /**
   * returns the optional processStep value
   */
  private static String getProcessStep( String namespace, Element element )
      throws CatalogClientException
  {
    Debug.debugMethodBegin();

    String processStep = null;
    try
    {
      ElementList el = XMLTools.getChildElementsByName( "DQ_DataQuality", namespace, element );
      el = XMLTools.getChildElementsByName( "lineage", namespace, el.item( 0 ) );
      if( el.getLength() > 0 )
      {
        el = XMLTools.getChildElementsByName( "LI_Lineage", namespace, el.item( 0 ) );
        el = XMLTools.getChildElementsByName( "processStep", namespace, el.item( 0 ) );
        if( el.getLength() > 0 )
        {
          el = XMLTools.getChildElementsByName( "LI_ProcessStep", namespace, el.item( 0 ) );
          processStep = XMLTools.getStringValue( "description", namespace, el.item( 0 ), null );
        }
      }
    }
    catch( Exception e )
    {
      throw new CatalogClientException( e.toString(), e );
    }

    Debug.debugMethodEnd();
    return processStep;
  }

  /**
   * returns the optional maintenanceAndUpdateFrequency value
   */
  private static String getMaintenanceAndUpdateFrequency( String namespace, Element element )
      throws CatalogClientException
  {
    Debug.debugMethodBegin();

    String mauf = null;
    try
    {
      ElementList el = XMLTools.getChildElementsByName( "MD_MaintenanceInformation", namespace,
          element );
      if( el.getLength() > 0 )
      {
        mauf = XMLTools.getStringValue( "maintenanceAndUpdateFrequency", namespace, el.item( 0 ),
            null );
      }
    }
    catch( Exception e )
    {
      throw new CatalogClientException( e.toString(), e );
    }

    Debug.debugMethodEnd();
    return mauf;
  }

  /**
   * returns the optional resourceConstraints value
   */
  private static String getResourceConstraints( String namespace, Element element )
      throws CatalogClientException
  {
    Debug.debugMethodBegin();

    String constraints = null;
    try
    {
      ElementList el = XMLTools.getChildElementsByName( "MD_LegalConstraints", namespace, element );
      if( el.getLength() > 0 )
      {
        constraints = XMLTools.getStringValue( "otherConstraints", namespace, el.item( 0 ), null );
      }
    }
    catch( Exception e )
    {
      throw new CatalogClientException( e.toString(), e );
    }

    Debug.debugMethodEnd();
    return constraints;
  }

  /**
   * returns the optional equivalentScale value
   */
  private static String getEquivalentScale( String namespace, Element element )
      throws CatalogClientException
  {
    Debug.debugMethodBegin();

    String equivalentScale = null;
    try
    {
      ElementList el = XMLTools.getChildElementsByName( "equivalentScale", namespace, element );
      if( el.getLength() > 0 )
      {
        equivalentScale = XMLTools.getRequiredStringValue( "denominator",
            "http://www.isotc211.org/iso19115/", el.item( 0 ) );
      }
    }
    catch( Exception e )
    {
      throw new CatalogClientException( e.toString(), e );
    }

    Debug.debugMethodEnd();
    return equivalentScale;
  }

  /**
   * returns the optional keyword values
   */
  private static String[] getDescriptiveKeywords( String namespace, Element element )
      throws CatalogClientException
  {
    Debug.debugMethodBegin();

    String[] keywords = null;
    try
    {
      ElementList el = XMLTools.getChildElementsByName( "MD_Keywords", namespace, element );
      el = XMLTools.getChildElementsByName( "keyword", namespace, el.item( 0 ) );
      keywords = new String[el.getLength()];
      for( int i = 0; i < keywords.length; i++ )
      {
        keywords[i] = XMLTools.getStringValue( el.item( i ) );
      }

    }
    catch( Exception e )
    {
      throw new CatalogClientException( e.toString(), e );
    }

    Debug.debugMethodEnd();
    return keywords;
  }

  private static OnlineTransferOption[] getOnlineTransferOptions( String namespace, ElementList el )
      throws Exception
  {
    Debug.debugMethodBegin();
    OnlineTransferOption[] onlineTransferOptions = new OnlineTransferOption[el.getLength()];
    for( int i = 0; i < el.getLength(); i++ )
    {
      Element elem = XMLTools.getChildByName( "name", namespace, el.item( i ) );
      String name = null;
      if( elem != null )
      {
        name = XMLTools.getStringValue( elem );
      }
      elem = XMLTools.getChildByName( "description", namespace, el.item( i ) );
      String description = null;
      if( elem != null )
      {
        description = XMLTools.getStringValue( elem );
      }
      elem = XMLTools.getChildByName( "linkage", namespace, el.item( i ) );
      String tmp = null;
      URL linkage = null;
      if( elem != null )
      {
        tmp = XMLTools.getStringValue( elem );
        linkage = new URL( tmp );
      }
      onlineTransferOptions[i] = new OnlineTransferOption( name, linkage, description );
    }
    Debug.debugMethodEnd();
    return onlineTransferOptions;
  }

  //    public static void main(String[] args) throws Exception {
  //        java.io.FileReader reader = new java.io.FileReader( "c:/temp/test.xml" );
  //        Document doc = XMLTools.parse( reader );
  //        DetailedMetadata dm = MetadataFactory.createISO19115Full(
  // doc.getDocumentElement(), null );
  //        System.out.println(dm.getOnlineTransferOptions()[0].getLinkage());
  //    }

}