package com.bce.eind.core.profil.impl.devider;

import java.util.HashMap;

import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilPoint;

public class ProfilDevider implements IProfilDevider
{
  private DEVIDER_TYP m_Typ;

  private IProfilPoint m_point;

  private String m_label;

  private final HashMap<Object, Object> m_properties = new HashMap<Object, Object>();

  public ProfilDevider( final DEVIDER_TYP typ, final IProfilPoint point )
  {
    m_Typ = typ;
    m_point = point;
  }

  public Object getValueFor( final Object key )
  {
    return m_properties.get( key );
  }

  public IProfilPoint setPoint( IProfilPoint point )
  {
    final IProfilPoint oldPkt = m_point;
    m_point = point;
    return oldPkt;
  }

  public void setValueFor( final Object key, final Object value )
  {
    m_properties.put( key, value );
  }

  public IProfilPoint getPoint( )
  {
    return m_point;
  }

  public DEVIDER_TYP getTyp( )
  {
    return m_Typ;
  }

  public String getLabel( )
  {
    return m_label;
  }

}
