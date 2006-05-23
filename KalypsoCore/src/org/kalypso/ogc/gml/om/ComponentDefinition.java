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

import java.math.BigDecimal;
import java.util.Date;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
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
    final Class valueClass = m_representationType.getTypeHandler().getValueClass();

    if( Number.class.isAssignableFrom( valueClass ) )
    {
      final String unit = "";
      return new ValueComponent( m_name, m_desc, valueClass, unit );
    }

    if( Date.class.isAssignableFrom( valueClass ) )
    {
      final String tzName = "";
      return new DateComponent( m_name, m_desc, valueClass, tzName );
    }

    return new Component( m_name, m_desc, valueClass );
  }

  /**
   * Creates an instance of ComponentDefinition that best fits the given component
   */
  public static ComponentDefinition create( final IComponent component )
  {
    final Class< ? > valueClass = component.getValueClass();
    
    if( ValueComponent.class.isAssignableFrom( component.getClass() ) )
    {
      final RepresentationType rt = new RepresentationType( KIND.Number, new QName( NS.XSD_SCHEMA, toTypeName( valueClass ) ) );
      return new ComponentDefinition( component.getName(), component.getDescription(), rt );
    }

    if( DateComponent.class.isAssignableFrom( component.getClass() ) )
    {
      final RepresentationType rt = new RepresentationType( KIND.SimpleType, new QName( NS.XSD_SCHEMA, "dateTime" ) );
      return new ComponentDefinition( component.getName(), component.getDescription(), rt );
    }

    final RepresentationType rt = new RepresentationType( toKindType( valueClass ), new QName( NS.XSD_SCHEMA, toTypeName( valueClass ) ) );
    return new ComponentDefinition( component.getName(), component.getDescription(), rt );
  }

  /**
   * TODO extend this for more types
   */
  private static String toTypeName( final Class< ? > valueClass )
  {
    if( valueClass == BigDecimal.class )
      return "decimal";
    
    if( valueClass == Boolean.class )
      return "boolean";
    
    if( valueClass == XMLGregorianCalendar.class )
      return "dateTime";

    return "string";
  }

  /**
   * TODO extend this for more types
   */
  private static KIND toKindType( final Class< ? > valueClass )
  {
    if( valueClass == Boolean.class )
      return KIND.Boolean;
    
    if( valueClass == XMLGregorianCalendar.class )
      return KIND.SimpleType;

    return KIND.Word;
  }
}
