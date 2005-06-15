package com.bce.eind.core.profil;

public final class ProfilChange
{
  private final IProfilPoint m_point;

  private final ProfilPointProperty m_column;

  private final double m_newValue;

  public ProfilChange( final IProfilPoint p, final ProfilPointProperty column, final double newValue )
  {
    m_point = p;
    m_column = column;
    m_newValue = newValue;
  }

  /**
   * @return Returns the column.
   */
  public ProfilPointProperty getColumn( )
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