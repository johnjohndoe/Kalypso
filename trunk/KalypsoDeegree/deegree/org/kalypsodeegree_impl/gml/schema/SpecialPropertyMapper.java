/*
 * Created on 09.02.2005
 *  
 */
package org.kalypsodeegree_impl.gml.schema;

import java.util.HashMap;

/**
 * @author kuepfer
 */
public class SpecialPropertyMapper
{
  // TODO move to Kalypso-Commons, give a name like CastUtilities, work on Class objects instead of typenames
  private static SpecialPropertyMapper m_instance;

  private static HashMap m_map = new HashMap();
  static
  {
    m_instance = new SpecialPropertyMapper();

    m_instance.register( m_instance.new SpecialMapper( String.class, Integer.class )
    {
      public Object map( Object srcObject )
      {
        return new Integer( ((String) srcObject).trim() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( String.class, Double.class )
    {
      public Object map( Object srcObject )
      {
        return new Double( ((String) srcObject).trim() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( String.class, Float.class )
    {
      public Object map( Object srcObject )
      {
        return new Float( ((String) srcObject).trim() );
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Integer.class, Double.class )
    {
      public Object map( Object srcObject )
      {
        return new Double( ((Integer) srcObject).doubleValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( Long.class, Double.class )
    {
      public Object map( Object srcObject )
      {
        return new Double( ((Long) srcObject).doubleValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( Float.class, Double.class )
    {
      public Object map( Object srcObject )
      {
        return new Double( ((Float) srcObject).doubleValue() );
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Double.class, Integer.class )
    {
      public Object map( Object srcObject )
      {
        return new Integer( ((Double) srcObject).intValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( Double.class, Long.class )
    {
      public Object map( Object srcObject )
      {
        return new Long( ((Double) srcObject).longValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( Double.class, Float.class )
    {
      public Object map( Object srcObject )
      {
        return new Float( ((Double) srcObject).floatValue() );
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Double.class, String.class )
    {
      public Object map( Object srcObject )
      {
        return srcObject.toString();
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Integer.class, String.class )
    {
      public Object map( Object srcObject )
      {
        return srcObject.toString();
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Float.class, String.class )
    {
      public Object map( Object srcObject )
      {
        return srcObject.toString();
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Long.class, String.class )
    {
      public Object map( Object srcObject )
      {
        return srcObject.toString();
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( Float.class, Long.class )
    {
      public Object map( Object srcObject )
      {
        return new Long( ((Number) srcObject).longValue() );
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Long.class, Float.class )
    {
      public Object map( Object srcObject )
      {
        return new Float( ((Number) srcObject).floatValue() );
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Integer.class, Long.class )
    {
      public Object map( Object srcObject )
      {
        return new Long( ((Number) srcObject).longValue() );
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( Long.class, Integer.class )
    {
      public Object map( Object srcObject )
      {
        return new Integer( ((Number) srcObject).intValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( Integer.class, Float.class )
    {
      public Object map( Object srcObject )
      {
        return new Float( ((Number) srcObject).floatValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( Float.class, Integer.class )
    {
      public Object map( Object srcObject )
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
  private void register( SpecialMapper mapper )
  {
    m_map.put( mapper.getSrcType().getName() + mapper.getTargetType().getName(), mapper );
  }

  public static Object map( Class srcType, Class targetType, Object srcObject ) throws Exception
  {
    if( srcType.equals( targetType ) )
      return srcObject;
    final SpecialMapper mapper = (SpecialMapper) m_map.get( srcType.getName() + targetType.getName() );
    return mapper.map( srcObject );
  }

  public static boolean isValidMapping( Class srcType, Class targetType )
  {
    if( srcType.equals( targetType ) )
      return true;
    boolean isValid = m_map.containsKey( srcType.getName() + targetType.getName() );
    if( isValid )
      return true;
    return false;
  }

  /**
   * @param value
   * @param targetClass
   * @param doCastNull
   * @param mustClone
   *          TODO mustClone not supported yet
   * @return castedObject
   * @throws Exception
   */
  public static Object cast( final Object value, final Class targetClass, boolean doCastNull, boolean mustClone ) throws Exception
  {
    if( value == null && !doCastNull )
      throw new ClassCastException( "will not cast <null>" );
    if( value == null )
      return null;
    Class ty = value.getClass();
    Class t2 = targetClass;
    if( !isValidMapping( ty, t2 ) )
      throw new ClassCastException( "can not cast " + ty + " to " + t2 );
    return map( ty, t2, value );
  }

  private abstract class SpecialMapper
  {
    private final Class m_srcType;

    private final Class m_targetType;

    public SpecialMapper( Class srcType, Class targetType )
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