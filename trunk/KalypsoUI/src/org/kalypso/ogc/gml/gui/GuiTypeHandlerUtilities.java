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
package org.kalypso.ogc.gml.gui;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypsodeegree.model.XsdBaseTypeHandler;

/**
 * Helper methods for {@link org.kalypso.ogc.gml.gui.IGuiTypeHandler}
 * 
 * @author Belger
 */
public class GuiTypeHandlerUtilities
{
  private GuiTypeHandlerUtilities( )
  {
    // do not instantiate
  }

  public static void registerXSDSimpleTypeHandler( final ITypeRegistry<IGuiTypeHandler> guiRegistry ) throws TypeRegistryException
  {
    final ITypeRegistry<IMarshallingTypeHandler> marshallingRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();

    // we simply wrap all XsdBaseTypeHandler's
    final IMarshallingTypeHandler[] registeredTypeHandler = marshallingRegistry.getRegisteredTypeHandler( new IMarshallingTypeHandler[0] );
    for( final ITypeHandler handler : registeredTypeHandler )
    {
      if( handler instanceof XsdBaseTypeHandler )
      {
        final IGuiTypeHandler wrappedHandler = new XsdBaseGuiTypeHandler( (XsdBaseTypeHandler) handler );
        guiRegistry.registerTypeHandler( wrappedHandler );
      }
    }
    
    guiRegistry.registerTypeHandler( new XsdDateGuiTypeHandler( (XsdBaseTypeHandler) marshallingRegistry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "date" ) ) ) );
    
    guiRegistry.registerTypeHandler( new Gml3EnvelopeGuiTypeHandler( ) );
    guiRegistry.registerTypeHandler( new Gml3PointGuiTypeHandler( ) );
  }
}
