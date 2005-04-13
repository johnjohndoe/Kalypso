/*
 * Created on 22.02.2005
 */
package com.bce.eind.core.profil.impl.points.pointproperties;

import com.bce.eind.core.profil.IProfilPointProperty;


/**
 * @author kimwerner
 */
public class ProfilPointProperty implements IProfilPointProperty
{
private final String m_label;
private final boolean m_optional;
private final boolean m_visible;
private boolean m_interpolation;
private int m_precision;

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
  public ProfilPointProperty( final String label,final boolean optional,final boolean visible, final boolean interpolation, final int precision )
  {
    super();
    m_visible = visible;
    m_optional = optional;
    m_label = label;
    m_interpolation = interpolation;
    m_precision = precision;
  }
/**
 * @return Returns the optional.
 */
public boolean isOptional( )
{
  return m_optional;
}
/**
 * @return Returns the interpolation.
 */
public boolean isInterpolation( )
{
  return m_interpolation;
}
/**
 * @param interpolation The interpolation to set.
 */
public void setInterpolation( boolean interpolation )
{
  m_interpolation = interpolation;
}
/**
 * @return Returns the precision.
 */
public int getPrecision( )
{
  return m_precision;
}
/**
 * @param precision The precision to set.
 */
public void setPrecision( int precision )
{
  m_precision = precision;
}
}
