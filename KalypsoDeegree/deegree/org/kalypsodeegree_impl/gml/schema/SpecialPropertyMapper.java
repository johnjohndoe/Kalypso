/*
 * Created on 09.02.2005
 *  
 */
package org.kalypsodeegree_impl.gml.schema;

import java.util.HashMap;

/**
 * @author kuepfer
 *  
 */
public class SpecialPropertyMapper
{

  private static SpecialPropertyMapper m_instance;

  private static HashMap m_map = new HashMap();
  static
  {
    m_instance = new SpecialPropertyMapper();

    m_instance.register( m_instance.new SpecialMapper( "java.lang.String", "java.lang.Integer" )
    {
      public Object map( Object srcObject )
      {
        return new Integer( ( (String)srcObject ).trim() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( "java.lang.String", "java.lang.Double" )
    {
      public Object map( Object srcObject )
      {
        return new Double( ( (String)srcObject ).trim() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( "java.lang.String", "java.lang.Float" )
        {
          public Object map( Object srcObject )
          {
            return new Float( ( (String)srcObject ).trim() );
          }
        } );
    m_instance.register( m_instance.new SpecialMapper( "java.lang.Integer", "java.lang.Double" )
    {
      public Object map( Object srcObject )
      {
        return new Double( ( (Integer)srcObject ).doubleValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( "java.lang.Long", "java.lang.Double" )
    {
      public Object map( Object srcObject )
      {
        return new Double( ( (Long)srcObject ).doubleValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( "java.lang.Float", "java.lang.Double" )
    {
      public Object map( Object srcObject )
      {
        return new Double( ( (Float)srcObject ).doubleValue() );
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( "java.lang.Double", "java.lang.Integer" )
    {
      public Object map( Object srcObject )
      {
        return new Integer( ( (Double)srcObject ).intValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( "java.lang.Double", "java.lang.Long" )
    {
      public Object map( Object srcObject )
      {
        return new Long( ( (Double)srcObject ).longValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( "java.lang.Double", "java.lang.Float" )
    {
      public Object map( Object srcObject )
      {
        return new Float( ( (Double)srcObject ).floatValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( "java.lang.Float", "java.lang.Long" )
    {
      public Object map( Object srcObject )
      {
        return new Long( ( (Number)srcObject ).longValue() );
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( "java.lang.Long", "java.lang.Float" )
    {
      public Object map( Object srcObject )
      {
        return new Float( ( (Number)srcObject ).floatValue() );
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( "java.lang.Integer", "java.lang.Long" )
    {
      public Object map( Object srcObject )
      {
        return new Long( ( (Number)srcObject ).longValue() );
      }
    } );
    m_instance.register( m_instance.new SpecialMapper( "java.lang.Long", "java.lang.Integer" )
    {
      public Object map( Object srcObject )
      {
        return new Integer( ( (Number)srcObject ).intValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( "java.lang.Integer", "java.lang.Float" )
    {
      public Object map( Object srcObject )
      {
        return new Float( ( (Number)srcObject ).floatValue() );
      }
    } );

    m_instance.register( m_instance.new SpecialMapper( "java.lang.Float", "java.lang.Integer" )
    {
      public Object map( Object srcObject )
      {
        return new Integer( ( (Number)srcObject ).intValue() );
      }
    } );

  }

  private SpecialPropertyMapper()
  {

  // TODO Auto-generated constructor stub
  }

  /**
   * @param mapper
   */
  private void register( SpecialMapper mapper )
  {
    m_map.put( mapper.getSrcType() + mapper.getTargetType(), mapper );
  }

  public static Object map( String srcType, String targetType, Object srcObject ) throws Exception
  {
    if( srcType.equals( targetType ) )
      return srcObject;
    SpecialMapper mapper = (SpecialMapper)m_map.get( srcType + targetType );
    return mapper.map( srcObject );
  }

  public static boolean isValidMapping( String srcType, String targetType )
  {
    if( srcType.equals( targetType ) )
      return true;
    boolean isValid = m_map.containsKey( srcType + targetType );
    if( isValid )
      return true;
    return false;
  }

  private abstract class SpecialMapper
  {
    private final String m_srcType;

    private final String m_targetType;

    public SpecialMapper( String srcType, String targetType )
    {
      m_srcType = srcType;
      m_targetType = targetType;
    }

    public String getSrcType()
    {
      return m_srcType;
    }

    public String getTargetType()
    {
      return m_targetType;
    }

    public abstract Object map( Object srcObject );

  }
}