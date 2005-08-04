package com.bce.eind.core.profil;

public final class PointChange
{
  private final IProfilPoint m_point;

  private final PointProperty m_column;

  private final double m_newValue;

  public PointChange( final IProfilPoint p, final PointProperty column, final double newValue )
  {
    m_point = p;
    m_column = column;
    m_newValue = newValue;
  }

  /**
   * @return Returns the column.
   */
  public PointProperty getColumn( )
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