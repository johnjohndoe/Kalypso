package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

public final class PointChange
{
  private final IProfilPoint m_point;

  private final POINT_PROPERTY m_column;

  private final double m_newValue;

  public PointChange( final IProfilPoint p, final POINT_PROPERTY column, final double newValue )
  {
    m_point = p;
    m_column = column;
    m_newValue = newValue;
  }

  /**
   * @return Returns the column.
   */
  public POINT_PROPERTY getColumn( )
  {
    return m_column;
  }

  /**
   * @return Returns the newValue.
   */
  public double getNewValue( )
  {
    return m_newValue;
  }

  /**
   * @return Returns the point.
   */
  public IProfilPoint getPoint( )
  {
    return m_point;
  }
}