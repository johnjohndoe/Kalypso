package org.kalypso.ogc.sensor.template;

import java.util.EventObject;

/**
 * @author schlienger
 */
public class TemplateEvent extends EventObject
{
  public final static int TYPE_ADD = 0;

  public final static int TYPE_REMOVE = 1;
  
  public final static int TYPE_REMOVE_ALL = 2;

  private final Object m_obj;

  private final int m_type;

  public TemplateEvent( final Object src, final Object obj, final int type )
  {
    super( src );
    m_obj = obj;
    m_type = type;
  }

  public Object getObject()
  {
    return m_obj;
  }

  public int getType()
  {
    return m_type;
  }
}