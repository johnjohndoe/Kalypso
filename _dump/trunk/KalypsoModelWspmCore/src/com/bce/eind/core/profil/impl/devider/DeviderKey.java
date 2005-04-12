/*
 * Created on 23.02.2005
 */
package com.bce.eind.core.profil.impl.devider;

import com.bce.eind.core.profil.impl.points.ProfilPointProperty;

/**
 * @author kimwerner
 */
public class DeviderKey
{
 private final ProfilPointProperty m_columnKey;
 private final int m_value;
 
  public DeviderKey( final ProfilPointProperty columnKey, final int value )
  {
    super();
    m_columnKey = columnKey;
    m_value = value;
  }
  public DeviderKey( final ProfilPointProperty columnKey)
  {
    super();
    m_columnKey = columnKey;
    m_value = 0xFFFF;
   }
/**
 * @return Returns the label.
 */
public ProfilPointProperty getColumnKey( )
{
  return m_columnKey;
}
/**
 * @return Returns the value.
 */
public int getValue( )
{
  return m_value;
}
}
