/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypsodeegree.model.geometry;

import java.net.URL;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.types.AbstractGM_EnvelopeBindingTypeHandler;
import org.kalypso.gmlschema.types.JAXBContextProvider;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypso.gmlschema.types.UnmarshallResultEater;
import org.kalypsodeegree_impl.model.geometry.AdapterBindingToValue;
import org.kalypsodeegree_impl.model.geometry.AdapterGmlIO;
import org.kalypsodeegree_impl.model.geometry.AdapterValueToGMLBinding;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.ext.LexicalHandler;

/**
 * a generic typehandler for GM_Object geometries based on a bindingtypehandler<br>
 * is wraps binding geometries to GM_Object geometries
 * 
 * @author doemming
 */
public class GM_EnvelopeBindingTypeHandler extends AbstractGM_EnvelopeBindingTypeHandler
{
  private final QName m_xmlTagNameGML2 = new QName( NS.GML2, "Box" );

  private final QName m_xmlTagNameGML3 = new QName( NS.GML3, "Envelope" );

  public GM_EnvelopeBindingTypeHandler( final JAXBContextProvider jaxbContextProvider, final QName xmlTypeQName, final Class< ? > gm_objectClass, final boolean isGeometry )
  {
    super( jaxbContextProvider, xmlTypeQName, gm_objectClass, isGeometry, false, true );
  }

  /**
   * @see org.kalypso.gmlschema.types.GenericBindingTypeHandler#unmarshal(org.xml.sax.XMLReader,
   *      org.kalypso.contribs.java.net.IUrlResolver, org.kalypso.gmlschema.types.MarshalResultEater)
   */
  public void unmarshal( final XMLReader xmlReader, final URL context, final UnmarshallResultEater marshalResultEater, final String gmlVersion ) throws TypeRegistryException
  {
    final UnmarshallResultEater eater = new UnmarshallResultEater()
    {
      public void unmarshallSuccesful( final Object bindingGeometry ) throws SAXParseException
      {
        final Object geometryValue;
        try
        {
          final Class< ? > geometryClass = getValueClass();
          // TODO
          final AdapterBindingToValue bindingToGM_ObjectAdapter = AdapterGmlIO.getGMLBindingToGM_ObjectAdapter( gmlVersion );
          geometryValue = bindingToGM_ObjectAdapter.wrapFromBinding( bindingGeometry, geometryClass );
          // geometryValue = BindingToValueAdapter_GML31.createGM_Object( bindingGeometry, geometryClass );
          marshalResultEater.unmarshallSuccesful( geometryValue );
        }
        catch( final Exception e )
        {
          throw new SAXParseException( e.getLocalizedMessage(), null, e );
        }
      }
    };
    final QName xmlTagName = getXMLTagNameForGMLVersion( gmlVersion );
    unmarshal( xmlTagName, xmlReader, eater, gmlVersion );
  }

  /**
   * @see org.kalypso.gmlschema.types.GenericBindingTypeHandler#marshal(java.lang.Object, org.xml.sax.ContentHandler,
   *      org.xml.sax.ext.LexicalHandler, java.net.URL)
   */
  public void marshal( final QName propQName, final Object geometry, final ContentHandler contentHandler, final LexicalHandler lexicalHandler, final URL context, final String gmlVersion ) throws TypeRegistryException
  {
    try
    {
      final AdapterValueToGMLBinding valueToGMLBindingAdapter = AdapterGmlIO.getGM_ObjectToGMLBindingAdapter( gmlVersion );
      final Object bindingObject = valueToGMLBindingAdapter.wrapToBinding( (GM_Envelope) geometry );
      // final AbstractGeometryType wrappedValue = GMLBindingGM_ObjectAdapter_GML31.createBindingGeometryType(
      // gmlVersion, geometry );
      final QName xmlTagName = getXMLTagNameForGMLVersion( gmlVersion );
      super.marshal( xmlTagName, propQName, bindingObject, contentHandler, gmlVersion );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new TypeRegistryException( e );
    }
  }

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#cloneObject(java.lang.Object)
   */
  public Object cloneObject( final Object objectToClone, final String gmlVersion ) throws CloneNotSupportedException
  {
    try
    {
      if( objectToClone == null )
        return null;
      final GM_Envelope geometry = (GM_Envelope) objectToClone;

      final AdapterValueToGMLBinding objectToGMLBindingAdapter = AdapterGmlIO.getGM_ObjectToGMLBindingAdapter( gmlVersion );
      final AdapterBindingToValue bindingToGM_ObjectAdapter = AdapterGmlIO.getGMLBindingToGM_ObjectAdapter( gmlVersion );

      final Object bindingGeometry = objectToGMLBindingAdapter.wrapToBinding( geometry );
      final QName xmlTagName = getXMLTagNameForGMLVersion( gmlVersion );
      final Object clonedBindingGeometry = cloneObject( bindingGeometry, xmlTagName );

      final Class< ? > geometryClass = getValueClass();
      return bindingToGM_ObjectAdapter.wrapFromBinding( clonedBindingGeometry, geometryClass );
    }
    catch( final GM_Exception e )
    {
      throw new CloneNotSupportedException( e.getLocalizedMessage() );
    }
  }

  private QName getXMLTagNameForGMLVersion( final String gmlVersion )
  {
    if( gmlVersion != null && gmlVersion.startsWith( "3" ) )
      return m_xmlTagNameGML3;
    return m_xmlTagNameGML2; // gml2
  }

}
