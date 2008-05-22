/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree.model.typeHandler;

import java.net.URL;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

import org.kalypso.gmlschema.types.GenericBindingTypeHandler;
import org.kalypso.gmlschema.types.JAXBContextProvider;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypso.gmlschema.types.UnmarshallResultEater;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.geometry.AdapterBindingToValue;
import org.kalypsodeegree_impl.model.geometry.AdapterGmlIO;
import org.kalypsodeegree_impl.model.geometry.AdapterValueToGMLBinding;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;

/**
 * a generic typehandler for GM_Object geometries based on a bindingtypehandler<br>
 * is wraps binding geometries to GM_Object geometries
 * 
 * @author doemming
 */
public class GenericGM_ObjectBindingTypeHandler extends GenericBindingTypeHandler
{
  public GenericGM_ObjectBindingTypeHandler( final JAXBContextProvider jaxbContextProvider, final QName xmlTypeQName, final QName xmlTagQName, final Class< ? > gm_objectClass, final boolean isGeometry )
  {
    super( jaxbContextProvider, xmlTypeQName, xmlTagQName, gm_objectClass, isGeometry, false, true );
  }

  /**
   * @see org.kalypso.gmlschema.types.GenericBindingTypeHandler#unmarshal(org.xml.sax.XMLReader,
   *      org.kalypso.contribs.java.net.IUrlResolver, org.kalypso.gmlschema.types.MarshalResultEater)
   */
  @Override
  public void unmarshal( final XMLReader xmlReader, final URL context, final UnmarshallResultEater marshalResultEater, final String gmlVersion ) throws TypeRegistryException
  {
    final UnmarshallResultEater eater = new UnmarshallResultEater()
    {
      public void unmarshallSuccesful( final Object bindingGeometry ) throws SAXParseException
      {
        Object geometryValue = null;
        try
        {
          final Class< ? > geometryClass = getValueClass();

          try
          {
            final AdapterBindingToValue bindingToGM_ObjectAdapter = AdapterGmlIO.getGMLBindingToGM_ObjectAdapter( gmlVersion );
            geometryValue = bindingToGM_ObjectAdapter.wrapFromBinding( bindingGeometry, geometryClass );
          }
          catch( Exception e )
          {
            // try to load with other gml version
            if( gmlVersion.startsWith( "2" ) )
            {
              e.printStackTrace();
              final AdapterBindingToValue bindingToGM_ObjectAdapter = AdapterGmlIO.getGMLBindingToGM_ObjectAdapter( "3.1" );
              geometryValue = bindingToGM_ObjectAdapter.wrapFromBinding( bindingGeometry, geometryClass );
            }
            else
            {
              throw e;
            }
          }

          marshalResultEater.unmarshallSuccesful( geometryValue );
        }
        catch( final Exception e )
        {
          throw new SAXParseException( e.getLocalizedMessage(), null, e );
        }
      }
    };

    super.unmarshal( xmlReader, context, eater, gmlVersion );
  }

  /**
   * @see org.kalypso.gmlschema.types.GenericBindingTypeHandler#marshal(java.lang.Object, org.xml.sax.ContentHandler,
   *      org.xml.sax.ext.LexicalHandler, java.net.URL)
   */
  @Override
  public void marshal( final QName propQName, final Object geometry, final XMLReader reader, final URL context, final String gmlVersion ) throws SAXException
  {
    try
    {
      final AdapterValueToGMLBinding valueToGMLBindingAdapter = AdapterGmlIO.getGM_ObjectToGMLBindingAdapter( gmlVersion );
      final Object geomObject = valueToGMLBindingAdapter.wrapToBinding( (GM_Object) geometry );
      final JAXBElement< ? extends Object> geomElement = valueToGMLBindingAdapter.createJAXBGeometryElement( geomObject );

      super.marshal( propQName, geomElement, reader, gmlVersion );
    }
    catch( final GM_Exception e )
    {
      throw new SAXException( e );
    }
  }

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#cloneObject(java.lang.Object)
   */
  @Override
  public Object cloneObject( final Object objectToClone, final String gmlVersion ) throws CloneNotSupportedException
  {
    try
    {
      // TODO: the unmarshalling/marshalling (especially the adapting to the bindng stuff)
      // takes veeeery long!
      // better implement/fix the clone method of GM_Object's
      final GM_Object geometry = (GM_Object) objectToClone;

      final AdapterValueToGMLBinding objectToGMLBindingAdapter = AdapterGmlIO.getGM_ObjectToGMLBindingAdapter( gmlVersion );
      final AdapterBindingToValue bindingToGM_ObjectAdapter = AdapterGmlIO.getGMLBindingToGM_ObjectAdapter( gmlVersion );

      final Object geoObject = objectToGMLBindingAdapter.wrapToBinding( geometry );

      final Object clonedBindingGeometry = super.cloneObject( geoObject, gmlVersion );

      final Class< ? > geometryClass = getValueClass();
      return bindingToGM_ObjectAdapter.wrapFromBinding( clonedBindingGeometry, geometryClass );
    }
    catch( final GM_Exception e )
    {
      throw new CloneNotSupportedException( e.getLocalizedMessage() );
    }
  }

}
