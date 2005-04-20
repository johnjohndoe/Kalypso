/*
 * Created on 22.02.2005
 */
package com.bce.eind.core.profil.impl.points;

import com.bce.eind.core.profil.IProfilPointProperty;
import com.bce.eind.core.profil.IProfil.POINT_PROPERTY;

/**
 * @author kimwerner
 */
public class ProfilPointProperty implements IProfilPointProperty
{
  private final String m_label;

  private final boolean m_optional;

  private final boolean m_visible;

  private final boolean m_interpolation;

  private final int m_precision;
  
  private final POINT_PROPERTY m_enumID;

  public ProfilPointProperty( final String label, final boolean optional, final boolean visible,
      final boolean interpolation, final int precision, final POINT_PROPERTY enumID )
  {
    m_visible = visible;
    m_optional = optional;
    m_label = label;
    m_interpolation = interpolation;
    m_precision = precision;
    m_enumID = enumID;
  }

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
   * @return Returns the precision.
   */
  public int getPrecision( )
  {
    return m_precision;
  }

  /* (non-Javadoc)
   * @see com.bce.eind.core.profil.IProfilPointProperty#getID()
   */
  public POINT_PROPERTY getID( )
  {
    return m_enumID;
  }
}
