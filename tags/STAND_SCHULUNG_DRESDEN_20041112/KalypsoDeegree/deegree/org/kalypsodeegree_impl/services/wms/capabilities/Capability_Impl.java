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
package org.deegree_impl.services.wms.capabilities;

import org.deegree.services.capabilities.CException;
import org.deegree.services.wms.capabilities.Capability;
import org.deegree.services.wms.capabilities.Layer;
import org.deegree.services.wms.capabilities.Request;
import org.deegree.services.wms.capabilities.UserDefinedSymbolization;
import org.deegree.xml.DOMPrinter;
import org.deegree.xml.Marshallable;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * The <Capability>element of the Capabilities XML names the actual operations
 * that are supported by the service instance, the output formats offered for
 * those operations, and the URL prefix for each operation. The XML DTD includes
 * placeholders for Distributed Computing Platforms other than HTTP, and request
 * methods other that HTTP GET, but currently only HTTP GET is defined for a
 * basic WMS.
 * <p>
 * </p>
 * Ignorable vendor-specific elements may be included. An SLD WMS would also
 * include a <UserDefinedSymbolization>element and URLs for HTTP POST requests.
 * 
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$
 */
public class Capability_Impl implements Capability, Marshallable
{
  private CException exception = null;

  private Document vendorSpecificCapabilities = null;

  private Layer layer = null;

  private Request request = null;

  private UserDefinedSymbolization userDefinedSymbolization = null;

  /**
   * default constructor
   */
  Capability_Impl()
  {}

  /**
   * constructor initializing the class with the capabilities
   */
  Capability_Impl( Request request, CException exception, Document vendorSpecificCapabilities,
      UserDefinedSymbolization userDefinedSymbolization, Layer layer )
  {
    setRequest( request );
    setException( exception );
    setVendorSpecificCapabilities( vendorSpecificCapabilities );
    setUserDefinedSymbolization( userDefinedSymbolization );
    setLayer( layer );
  }

  /**
   * Available WMS Operations are listed in a Request element.
   */
  public Request getRequest()
  {
    return request;
  }

  /**
   * sets available WMS Operations listed in a Request element.
   */
  public void setRequest( Request request )
  {
    this.request = request;
  }

  /**
   * returns the format where exceptions will be returned
   */
  public CException getException()
  {
    return exception;
  }

  /**
   * sets the format where exceptions will be returned
   */
  public void setException( CException exception )
  {
    this.exception = exception;
  }

  /**
   * returns vendor specific capabilities that are not common to wms as dom
   * document.
   */
  public Document getVendorSpecificCapabilities()
  {
    return vendorSpecificCapabilities;
  }

  /**
   * sets vendor specific capabilities that are not common to wms as dom
   * document.
   */
  public void setVendorSpecificCapabilities( Document vendorSpecificCapabilities )
  {
    this.vendorSpecificCapabilities = vendorSpecificCapabilities;
  }

  /**
   * Optional user-defined symbolization (used only by SLD-enabled WMSes).
   */
  public UserDefinedSymbolization getUserDefinedSymbolization()
  {
    return userDefinedSymbolization;
  }

  /**
   * sets the user-defined symbolization
   */
  public void setUserDefinedSymbolization( UserDefinedSymbolization userDefinedSymbolization )
  {
    this.userDefinedSymbolization = userDefinedSymbolization;
  }

  /**
   * returns the top level layer that may encloses several more layers and layer
   * hierachies available by a map server. If no layer is available
   * <tt>null</tt> will be returned.
   */
  public Layer getLayer()
  {
    return layer;
  }

  /**
   * Returns the Layer identified by the submitted name. If no Layer matches the
   * name <tt>null</tt> will be returned.
   * 
   * @param name
   *          name of the requested layer
   * 
   * @return a layer object or <tt>null</tt>
   *  
   */
  public Layer getLayer( String name )
  {
    Layer lay = null;

    if( layer.getName() != null && name.equals( layer.getName() ) )
    {
      lay = layer;
    }
    else
    {
      lay = getLayer( name, layer.getLayer() );
    }

    return lay;
  }

  /**
   * recursion over all layers to find the layer that matches the submitted
   * name. If no layer can be found that fullfills the condition <tt>null</tt>
   * will be returned.
   * 
   * @param name
   *          name of the layer to be found
   * @param layers
   *          list of searchable layers
   * 
   * @return a layer object or <tt>null</tt>
   */
  private Layer getLayer( String name, Layer[] layers )
  {
    Layer lay = null;

    if( layers != null )
    {
      for( int i = 0; i < layers.length; i++ )
      {
        if( layers[i].getName() != null && name.equals( layers[i].getName() ) )
        {
          lay = layers[i];
          break;
        }
        else
        {
          lay = getLayer( name, layers[i].getLayer() );
          if( lay != null )
            break;
        }
      }
    }

    return lay;
  }

  /**
   * sets the top level layer that may encloses several more layers and layer
   * hierachies available by a map server. If no layer is available
   * <tt>null</tt> will be returned.
   */
  public void setLayer( Layer layer )
  {
    this.layer = layer;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "request = " + request + "\n";
    ret += ( "exception = " + exception + "\n" );
    ret += ( "vendorSpecificCapabilities = " + vendorSpecificCapabilities + "\n" );
    ret += ( "userDefinedSymbolization = " + userDefinedSymbolization + "\n" );
    ret += ( "layer = " + layer + "\n" );
    return ret;
  }

  /**
   * Returns an XML representation of this object.
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer();

    sb.append( "<Capability>" ).append( ( (Marshallable)request ).exportAsXML() ).append(
        ( (Marshallable)exception ).exportAsXML() );

    if( vendorSpecificCapabilities != null )
    {
      Element element = vendorSpecificCapabilities.getDocumentElement();
      sb.append( DOMPrinter.nodeToString( element, "UTF-8" ) );
    }

    if( userDefinedSymbolization != null )
    {
      sb.append( ( (Marshallable)userDefinedSymbolization ).exportAsXML() );
    }

    if( layer != null )
    {
      sb.append( ( (Marshallable)layer ).exportAsXML() );
    }

    sb.append( "</Capability>" );

    return sb.toString();
  }
}