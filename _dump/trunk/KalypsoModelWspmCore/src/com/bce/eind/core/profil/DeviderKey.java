/*
 * Created on 23.02.2005
 */
package com.bce.eind.core.profil;


/**
 * @author kimwerner
 */
public class DeviderKey
{
 private final ProfilPointProperty m_ProfilPointProperty;
 private final int m_value;
 
 public static final DeviderKey BORDVOLL_L = new DeviderKey( ProfilPointProperty.BORDVOLL, -1 );

 public static final DeviderKey BORDVOLL_R = new DeviderKey( ProfilPointProperty.BORDVOLL, 1 );

 public static final DeviderKey DURCHSTROEMTE_L = new DeviderKey( ProfilPointProperty.DURCHSTROEMTE, -1 );

 public static final DeviderKey DURCHSTROEMTE_R = new DeviderKey( ProfilPointProperty.DURCHSTROEMTE, 1 );

 public static final DeviderKey TRENNFLAECHE_L = new DeviderKey( ProfilPointProperty.TRENNFLAECHE, -1 );

 public static final DeviderKey TRENNFLAECHE_R = new DeviderKey( ProfilPointProperty.TRENNFLAECHE, 1 );

 
  private DeviderKey( final ProfilPointProperty pointProperty, final int value )
  {
    super();
    m_ProfilPointProperty = pointProperty;
    m_value = value;
  }
  public DeviderKey( final ProfilPointProperty pointProperty)
  {
    super();
    m_ProfilPointProperty = pointProperty;
    m_value = 0xFFFF;
   }
/**
 * @return Returns the label.
 */
public ProfilPointProperty getProfilPointProperty( )
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
