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
package org.deegree_impl.clients.wcasclient.control;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.RPCException;
import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.xml.ElementList;
import org.deegree.xml.XMLParsingException;
import org.deegree.xml.XMLTools;
import org.deegree_impl.clients.wcasclient.configuration.CSWClientConfiguration;
import org.deegree_impl.enterprise.control.AbstractSecuredListener;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.NetWorker;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

/**
 * This <tt>Listener</tt> reacts on 'initServiceAdministration' - events,
 * queries the WCAS and passes the service data on to be displayed by the JSP.
 * <p>
 * 
 * @author <a href="mschneider@lat-lon.de">Markus Schneider </a>
 */
public class InitServiceAdministrationListener extends AbstractSecuredListener
{

  public void performPrivilegedOperation( FormEvent event )
  {
    Debug.debugMethodBegin();

    try
    {
      URL catalogURL = null;

      // decode RPC-event
      if( event instanceof RPCWebEvent )
      {
        RPCWebEvent ev = (RPCWebEvent)event;
        RPCMethodCall rpcCall = ev.getRPCMethodCall();
        RPCParameter[] params = rpcCall.getParameters();

        if( params == null || params.length != 1 )
        {
          throw new RPCException( "Invalid RPC. Exactly one "
              + "'param'-element below 'params' is required." );
        }
        if( !( params[0].getValue() instanceof String ) )
        {
          throw new RPCException( "Invalid RPC. 'param'-element "
              + "below 'params' must contain a 'string'." );
        }
        catalogURL = CSWClientConfiguration.getInstance().getCatalogServerAddress(
            (String)params[0].getValue() );

      }
      else
      {
        throw new Exception( "Es wurde kein gültiger RPC-event empfangen." );
      }

      Set allServices = getBriefDescriptions( catalogURL );
      String[] serviceDetails = new String[14];
      for( int i = 0; i < serviceDetails.length; i++ )
      {
        serviceDetails[i] = "";
      }

      // display the first service
      String serviceId = null;
      Iterator it = allServices.iterator();

      if( it.hasNext() )
      {
        serviceId = ( (String[])it.next() )[0];
      }
      if( serviceId != null )
      {
        serviceDetails = getFullDescription( catalogURL, serviceId );
      }
      getRequest().setAttribute( "ALL_SERVICES", allServices );
      getRequest().setAttribute( "SERVICE_DETAILS", serviceDetails );
    }
    catch( Exception e )
    {
      getRequest().setAttribute( "SOURCE", this.getClass().getName() );
      getRequest().setAttribute(
          "MESSAGE",
          "Die Serviceadministration konnte nicht "
              + "initialisiert werden, da ein Fehler augetreten ist.<br><br>"
              + "Die Fehlermeldung lautet: <code>" + e.getMessage() + "</code>" );
      setNextPage( "admin_error.jsp" );
    }

    Debug.debugMethodEnd();
  }

  /**
   * Retrieves "brief descriptions" of all available services from the WCAS.
   * <p>
   * 
   * @param catalogURL
   * @return elements are arrays of Strings (title, id, type)
   * @throws IOException
   * @throws SAXException
   * @throws XMLParsingException
   */
  public static Set getBriefDescriptions( URL catalogURL ) throws IOException, SAXException,
      XMLParsingException
  {
    Debug.debugMethodBegin();

    TreeSet services = new TreeSet( new Comparator()
    {
      public int compare( Object o1, Object o2 )
      {
        if( o1 instanceof String[] && o2 instanceof String[] )
        {
          String id1 = ( (String[])o1 )[0];
          String id2 = ( (String[])o2 )[1];
          return id1.compareTo( id2 );
        }
        throw new ClassCastException( "Incompatible object types!" );
      }
    } );

    // build WCAS-request
    String briefRequest = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>"
        + "<GetRecord xmlns:ogc=\"http://www.opengis.net/ogc\""
        + " xmlns:gml=\"http://www.opengis.net/gml\" maxRecords=\"10\""
        + " outputFormat=\"XML\" outputRecType=\"ISO19119\" queryScope=\"0\""
        + " startPosition=\"-1\">" + "<Query typeName=\"Service\">"
        + "<PropertySet setName=\"Full\"/>" + "</Query>" + "</GetRecord>";

    // open connection and send request
    NetWorker netWorker = new NetWorker( catalogURL, briefRequest );

    // server response -> DOM
    InputStreamReader reader = new InputStreamReader( netWorker.getInputStream(), "UTF-8" );
    Document doc = XMLTools.parse( reader );
    reader.close();

    // extract service information from DOM
    Element searchResultElement = XMLTools.getRequiredChildByName( "searchResult", null, doc
        .getDocumentElement() );
    ElementList serviceElements = XMLTools.getChildElementsByName( "ISO19119", null,
        searchResultElement );

    for( int i = 0; i < serviceElements.getLength(); i++ )
    {
      Element serviceElement = serviceElements.item( i );
      String serviceId = XMLTools.getRequiredStringValue( "fileIdentifier", null, serviceElement );
      String serviceType = XMLTools.getRequiredStringValue( "serviceType", null, serviceElement );
      Element citationElement = XMLTools.getRequiredChildByName( "citation", null, serviceElement );
      String serviceName = XMLTools.getRequiredStringValue( "title", null, citationElement );
      String[] service = new String[]
      { serviceId, serviceType, serviceName };
      services.add( service );
    }

    Debug.debugMethodEnd();
    return services;
  }

  /**
   * Retrieves the full description for the specified service from the WCAS.
   * <p>
   * 
   * @param catalogURL
   * @param serviceId
   * @return String array (14 values) describing the service
   * @throws Exception
   */
  public static String[] getFullDescription( URL catalogURL, String serviceId ) throws Exception
  {
    Debug.debugMethodBegin();

    String[] details = new String[14];

    // build WCAS-request
    String fullRequest = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>"
        + "<GetRecord xmlns:ogc=\"http://www.opengis.net/ogc\""
        + " xmlns:gml=\"http://www.opengis.net/gml\" maxRecords=\"10\""
        + " outputFormat=\"XML\" outputRecType=\"ISO19119\" queryScope=\"0\""
        + " startPosition=\"-1\">" + "<Query typeName=\"Service\">"
        + "<PropertySet setName=\"Full\"/>" + "<ogc:Filter>" + "<ogc:PropertyIsEqualTo>"
        + "<ogc:PropertyName>ISO19119/fileIdentifier</ogc:PropertyName>" + "<ogc:Literal><![CDATA["
        + serviceId + "]]></ogc:Literal>" + "</ogc:PropertyIsEqualTo>" + "</ogc:Filter>"
        + "</Query>" + "</GetRecord>";

    // open connection and send request
    NetWorker netWorker = new NetWorker( catalogURL, fullRequest );

    // server response -> DOM
    InputStreamReader reader = new InputStreamReader( netWorker.getInputStream(), "UTF-8" );
    Document doc = XMLTools.parse( reader );
    reader.close();

    // extract service information from DOM
    Element searchResultElement = XMLTools.getRequiredChildByName( "searchResult", null, doc
        .getDocumentElement() );
    ElementList serviceElements = XMLTools.getChildElementsByName( "ISO19119", null,
        searchResultElement );
    if( serviceElements.getLength() != 1 )
    {
      throw new XMLParsingException(
          "Fehler in WCAS-Antwort zur Detailanfrage. Unerwartete Anzahl ("
              + serviceElements.getLength() + ") an ISO19119-Elementen." );
    }
    Element serviceElement = serviceElements.item( 0 );

    details[0] = serviceId;
    details[1] = XMLTools.getRequiredStringValue( "serviceType", null, serviceElement );
    details[2] = XMLTools.getRequiredStringValue( "serviceTypeVersion", null, serviceElement );

    Element citationElement = XMLTools.getChildByName( "citation", null, serviceElement );
    if( citationElement != null )
    {
      details[3] = XMLTools.getStringValue( "title", null, citationElement, "" );
      details[4] = XMLTools.getStringValue( "abstract", null, serviceElement, "" );

      Element pointOfContactElement = XMLTools.getChildByName( "pointOfContact", null,
          serviceElement );
      if( pointOfContactElement != null )
      {
        details[5] = XMLTools.getStringValue( "individualName", null, pointOfContactElement, "" );
        details[6] = XMLTools.getStringValue( "positionName", null, pointOfContactElement, "" );
        details[7] = XMLTools.getStringValue( "organizationName", null, pointOfContactElement, "" );
        details[8] = XMLTools.getStringValue( "onlineResource", null, pointOfContactElement, "" );

        Element contactInfoElement = XMLTools.getChildByName( "contactInfo", null,
            pointOfContactElement );
        if( contactInfoElement != null )
        {
          Element addressElement = XMLTools.getChildByName( "address", null, contactInfoElement );
          if( addressElement != null )
          {
            details[9] = XMLTools.getStringValue( "deliveryPoint", null, addressElement, "" );
            details[10] = XMLTools.getStringValue( "city", null, addressElement, "" );
            details[11] = XMLTools.getStringValue( "postalCode", null, addressElement, "" );
            details[12] = XMLTools.getStringValue( "country", null, addressElement, "" );
            details[13] = XMLTools.getStringValue( "electronicMailAddress", null, addressElement,
                "" );
          }
        }
      }
    }

    Debug.debugMethodEnd();
    return details;
  }
}