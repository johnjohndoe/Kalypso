/*
 * Created on 23.02.2005
 */
package com.bce.eind.core.profil.impl.devider;

import com.bce.eind.core.profil.IProfil.POINT_PROPERTY;

/**
 * @author kimwerner
 */
public class DeviderKey
{
 private final POINT_PROPERTY m_ProfilPointProperty;
 private final int m_value;
 
  public DeviderKey( final POINT_PROPERTY pointProperty, final int value )
  {
    super();
    m_ProfilPointProperty = pointProperty;
    m_value = value;
  }
  public DeviderKey( final POINT_PROPERTY pointProperty)
  {
    super();
    m_ProfilPointProperty = pointProperty;
    m_value = 0xFFFF;
   }
/**
 * @return Returns the label.
 */
public POINT_PROPERTY getProfilPointProperty( )
{
  return m_ProfilPointProperty;
}
/**
 * @return Returns the value.
 */
public int getValue( )
{
  return m_value;
}
}
