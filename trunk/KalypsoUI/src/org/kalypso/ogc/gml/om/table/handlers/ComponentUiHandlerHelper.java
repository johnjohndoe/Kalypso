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
package org.kalypso.ogc.gml.om.table.handlers;

import javax.xml.namespace.QName;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.gmlschema.swe.RepresentationType;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.ogc.gml.om.FeatureComponent;
import org.kalypso.template.featureview.ColumnDescriptor;
import org.kalypso.template.featureview.ColumnTypeDescriptor;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kuch
 */
public class ComponentUiHandlerHelper
{

  public static IComponentUiHandler[] createHandlers( final ColumnDescriptor[] descriptors )
  {
    throw new NotImplementedException();
  }

  public static IComponentUiHandler[] createHandlers( final ColumnTypeDescriptor[] columnTypeDescriptors )
  {
    throw new NotImplementedException();
  }

  public static IComponentUiHandler getHandler( final Feature feature )
  {
    final FeatureComponent component = new FeatureComponent( feature );
    final RepresentationType representationType = component.getRepresentationType();

    final QName valueTypeName = representationType.getValueTypeName();

    if( valueTypeName.equals( new QName( "http://www.w3.org/2001/XMLSchema", "dateTime" ) ) )
      return new ComponentUiDateHandler( component );
    else if( valueTypeName.equals( new QName( "http://www.w3.org/2001/XMLSchema", "double" ) ) )
      return new ComponentUiDoubleHandler( component );
    else if( valueTypeName.equals( new QName( "http://www.w3.org/2001/XMLSchema", "string" ) ) )
    {
      if( ComponentUtilities.restrictionContainsEnumeration( component.getRestrictions() ) )
        return new ComponentUiEnumerationHandler( component );

      return new ComponentUiStringHandler( component );
    }

    throw new NotImplementedException();
  }

  public static IComponentUiHandler getHandler( final IComponent component )
  {

// final QName valueTypeName = representationType.getValueTypeName();
//
// if( valueTypeName.equals( new QName( "http://www.w3.org/2001/XMLSchema", "dateTime" ) ) )
// return new ComponentUiDateHandler( component );
// else if( valueTypeName.equals( new QName( "http://www.w3.org/2001/XMLSchema", "double" ) ) )
// return new ComponentUiDoubleHandler( component );

    throw new NotImplementedException();
  }

}
