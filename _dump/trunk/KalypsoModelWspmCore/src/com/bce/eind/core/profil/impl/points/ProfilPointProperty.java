/*
 * Created on 22.02.2005
 */
package com.bce.eind.core.profil.impl.points;


/**
 * @author kimwerner
 */
public class ProfilPointProperty
{
private final String m_label;
private final boolean m_optional;
private final boolean m_visible;
//public HashMap extendedSerializerData;

/**
 * @return Returns the visible.
 */
public boolean isVisible( )
{
  return m_visible;
}
/**
 * @return Returns the label.
 */
public String getLabel( )
{
  return m_label;
}
  public ProfilPointProperty( final String label,final boolean optional,final boolean visible )
  {
    super();
    m_visible = visible;
    m_optional = optional;
    m_label = label;
  }
/**
 * @return Returns the optional.
 */
public boolean isOptional( )
{
  return m_optional;
}
}
