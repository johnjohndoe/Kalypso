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
package org.kalypsodeegree_impl.gml.schema;

import java.util.HashMap;

import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author kuepfer
 */
public class SpecialPropertyMapper
{
  // TODO move to Kalypso-NA, this makes only sense for this special case!
  private static SpecialPropertyMapper m_instance;

  private static HashMap<String, SpecialMapper> m_map = new HashMap<String, SpecialMapper>();
  static
  {
    m_instance = new SpecialPropertyMapper();

    m_instance.register( m_instance.new SpecialMapper( GM_Surface.class, GM_MultiSurface.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        final GM_Surface surface = (GM_Surface) srcObject;
        final GM_Surface[] surfaces = new GM_Surface[] { surface };
        return GeometryFactory.createGM_MultiSurface( surfaces, surface.getCoordinateSystem() );
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( GM_MultiSurface.class, GM_Surface.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        final GM_Surface[] surfaces = new GM_Surface[] { ((GM_MultiSurface) srcObject).getSurfaceAt( 0 ) };
        return surfaces[0];
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( GM_MultiCurve.class, GM_Curve.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        final GM_Curve[] curves = new GM_Curve[] { ((GM_MultiCurve) srcObject).getCurveAt( 0 ) };
        return curves[0];
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( String.class, Integer.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return new Integer( ((String) srcObject).trim() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( String.class, Double.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return new Double( ((String) srcObject).trim().replace( ',','.' ) );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( String.class, Float.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return new Float( ((String) srcObject).trim() );
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Integer.class, Double.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return new Double( ((Integer) srcObject).doubleValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( Long.class, Double.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return new Double( ((Long) srcObject).doubleValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( Float.class, Double.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return new Double( ((Float) srcObject).doubleValue() );
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Double.class, Integer.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return new Integer( ((Double) srcObject).intValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( Double.class, Long.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return new Long( ((Double) srcObject).longValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( Double.class, Float.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return new Float( ((Double) srcObject).floatValue() );
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Double.class, String.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return srcObject.toString();
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Integer.class, String.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return srcObject.toString();
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Float.class, String.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return srcObject.toString();
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Long.class, String.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return srcObject.toString();
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( Float.class, Long.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return new Long( ((Number) srcObject).longValue() );
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Long.class, Float.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return new Float( ((Number) srcObject).floatValue() );
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Integer.class, Long.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return new Long( ((Number) srcObject).longValue() );
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Long.class, Integer.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return new Integer( ((Number) srcObject).intValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( Integer.class, Float.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return new Float( ((Number) srcObject).floatValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( Float.class, Integer.class )
    {
      @Override
      public Object map( final Object srcObject )
      {
        return new Integer( ((Number) srcObject).intValue() );
      }
    } );

  }

  private SpecialPropertyMapper( )
  {

    // TODO Auto-generated constructor stub
  }

  /**
   * @param mapper
   */
  private void register( final SpecialMapper mapper )
  {
    m_map.put( mapper.getSrcType().getName() + mapper.getTargetType().getName(), mapper );
  }

  public static Object map( final Class srcType, final Class targetType, final Object srcObject ) throws Exception
  {
    if( srcType.equals( targetType ) )
      return srcObject;
    final SpecialMapper mapper = m_map.get( srcType.getName() + targetType.getName() );
    return mapper.map( srcObject );
  }

  public static boolean isValidMapping( final Class srcType, final Class targetType )
  {
    if( srcType.equals( targetType ) )
      return true;
    final boolean isValid = m_map.containsKey( srcType.getName() + targetType.getName() );
    if( isValid )
      return true;
    return false;
  }

  /**
   * @param value
   * @param targetClass
   * @param doCastNull
   * @param mustClone
   *            TODO mustClone not supported yet
   * @return castedObject
   * @throws Exception
   */
  public static Object cast( final Object value, final Class targetClass, final boolean doCastNull, final boolean mustClone ) throws Exception
  {
    if( value == null && !doCastNull )
      throw new ClassCastException( "will not cast <null>" );
    if( value == null )
      return null;
    final Class ty = value.getClass();
    final Class t2 = targetClass;
    if( !isValidMapping( ty, t2 ) )
      throw new ClassCastException( "can not cast " + ty + " to " + t2 );
    return map( ty, t2, value );
  }

  private abstract class SpecialMapper
  {
    private final Class m_srcType;

    private final Class m_targetType;

    public SpecialMapper( final Class srcType, final Class targetType )
    {
      m_srcType = srcType;
      m_targetType = targetType;
    }

    public Class getSrcType( )
    {
      return m_srcType;
    }

    public Class getTargetType( )
    {
      return m_targetType;
    }

    public abstract Object map( Object srcObject );

  }
}