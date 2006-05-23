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
package org.kalypso.ogc.gml.om;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.observation.result.Component;
import org.kalypso.observation.result.DateComponent;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.ValueComponent;
import org.kalypso.ogc.swe.RepresentationType;
import org.kalypso.ogc.swe.RepresentationType.KIND;
import org.kalypsodeegree.model.XsdBaseTypeHandler;

/**
 * @author schlienger
 */
public class ComponentDefinition
{
  private final RepresentationType m_representationType;

  private final String m_name;

  private final String m_desc;

  public ComponentDefinition( final String name, final String desc, final RepresentationType representationType )
  {
    m_name = name;
    m_desc = desc;
    m_representationType = representationType;
  }

  public String getName( )
  {
    return m_name;
  }

  public String getDescription( )
  {
    return m_desc;
  }

  public RepresentationType getRepresentationType( )
  {
    return m_representationType;
  }

  public XsdBaseTypeHandler<Object> getTypeHandler( )
  {
    return (XsdBaseTypeHandler<Object>) m_representationType.getTypeHandler();
  }

  /**
   * Creates an IComponent representation of this ComponentDefinition
   */
  public IComponent toComponent( )
  {
    final QName valueTypeName = m_representationType.getValueTypeName();

    if( XmlTypes.isNumber( valueTypeName ) )
    {
      final String unit = "";
      return new ValueComponent( m_name, m_desc, valueTypeName, unit );
    }

    if( XmlTypes.isDate( valueTypeName ) )
    {
      final String tzName = "";
      return new DateComponent( m_name, m_desc, valueTypeName, tzName );
    }

    return new Component( m_name, m_desc, valueTypeName );
  }

  /**
   * Creates an instance of ComponentDefinition that best fits the given component
   */
  public static ComponentDefinition create( final IComponent component )
  {
    final QName valueTypeName = component.getValueTypeName();

    String unit = "";
    String frame = "";
    String classification = "";

    if( component instanceof ValueComponent )
      unit = ((ValueComponent) component).getUnit();

    if( component instanceof DateComponent )
      frame = ((DateComponent) component).getTimezoneName();

    final RepresentationType rt = new RepresentationType( toKind( valueTypeName ), valueTypeName, unit, frame, classification );
    return new ComponentDefinition( component.getName(), component.getDescription(), rt );
  }

  /**
   * Finds the best KIND that suits the given QName
   */
  private static KIND toKind( final QName valueTypeName )
  {
    if( XmlTypes.XS_BOOLEAN.equals( valueTypeName ) )
      return KIND.Boolean;

    if( XmlTypes.isNumber( valueTypeName ) )
      return KIND.Number;

    if( XmlTypes.isDate( valueTypeName ) )
      return KIND.SimpleType;

    return KIND.Word;
  }
}
