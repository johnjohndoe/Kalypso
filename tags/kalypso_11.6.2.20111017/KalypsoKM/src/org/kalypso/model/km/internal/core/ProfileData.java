package org.kalypso.model.km.internal.core;

import java.math.BigDecimal;

import org.kalypso.model.km.internal.i18n.Messages;

public class ProfileData
{
  /**
   * Station [km]
   */
  private final BigDecimal m_station;

  private Row[] m_rows;

  private final String m_file;

  /**
   * Length [m]
   */
  private double m_length;

  /**
   * @param station
   *          This profiles station [km]
   */
  public ProfileData( final String file, final BigDecimal station )
  {
    m_file = file;
    m_station = station;
  }

  public String getFile( )
  {
    return m_file;
  }

  public double getLength( )
  {
    return m_length;
  }

  @Override
  public String toString( )
  {
    final StringBuffer result = new StringBuffer( Messages.getString( "org.kalypso.model.km.ProfileData.0" ) + m_station + Messages.getString( "org.kalypso.model.km.ProfileData.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    for( final Row row : m_rows )
    {
      result.append( row.toString() );
      result.append( "\n" ); //$NON-NLS-1$

    }
    return result.toString();
  }

  public BigDecimal getStation( )
  {
    return m_station;
  }

  public void set( final Row[] rowArray )
  {
    m_rows = rowArray;
  }

  public int getNumberKMValues( )
  {
    return m_rows.length - 1;
  }

  /**
   * @param lengthFactor
   *          The length factor is used to adjust the real length of the profile, in order to give the sum of all
   *          profile the length of the calculated strand, even if we calculate with a subsection of all profiles. If
   *          only one profile is present, the factor should be exactly the length of the section, we wil use this
   *          length for the singular profile.
   */
  public IKMValue getKMValue( final double lengthFactor, final int indexQfrom, final int indexQto )
  {
    final double length = getLength();

    final double adjustedLength = getAdjustedLength( lengthFactor, length );

    return new KMValue( adjustedLength, m_rows[indexQfrom], m_rows[indexQto] );
  }

  protected double getAdjustedLength( final double lengthFactor, final double length )
  {
    if( Double.isNaN( length ) )
      return lengthFactor;

    return length * lengthFactor;
  }

  public String isValidForKalypso( )
  {
    if( m_rows.length < 6 ) // not enough values for calculation
      return m_station + Messages.getString( "org.kalypso.model.km.ProfileData.3" ); //$NON-NLS-1$

    for( int i = 0; i < m_rows.length - 1; i++ )
    {
      if( (m_rows[i].getQ() - m_rows[i + 1].getQ()) > 0.009 )
      {
        // Änderung in der dritten Nachkommastelle O.K. -
        // Rechenungenauigkeiten im Hydraulikmodell
        return Messages.getString( "org.kalypso.model.km.ProfileData.4" ) + (i + 2); //$NON-NLS-1$
      }
    }

    // TODO: Check if values above bordfull is necessary!
    if( m_rows[m_rows.length - 1].getAlpha() >= 1.0 ) // no values above bordfull
      return Messages.getString( "org.kalypso.model.km.ProfileData.6" ); //$NON-NLS-1$

    for( int i = 0; i < m_rows.length - 1; i++ )
    {
      if( m_rows[i].getSlope() < 0.0 )
      {
        return Messages.getString( "org.kalypso.model.km.ProfileData.7" ) + (i + 2) //$NON-NLS-1$ 
        + Messages.getString( "org.kalypso.model.km.ProfileData.9" ); //$NON-NLS-1$
      }
    }

    return null;
  }

  public double findQBordvoll( )
  {
    for( final Row row : m_rows )
    {
      final double qforeland = row.getQforeland();
      if( qforeland > 0 )
        return row.getQfull();
    }

    // TODO: check: using maxQ if we have no q bordvoll here
    return m_rows[m_rows.length - 1].getQfull();
  }

  /**
   * Returns the index of qBordvoll. We are currently using the index of the biggest q below qBordvoll.<br/>
   * This method is independent of the actual data and only depends on the existing discharges (equal for all profiles).
   */
  public int getQBordvollIndex( final double qBordvoll )
  {
    for( int i = 0; i < m_rows.length; i++ )
    {
      if( m_rows[i].getQfull() > qBordvoll )
      {
        return i - 1;
      }
    }

    return -1;
  }

  void setLength( final double length )
  {
    m_length = length;
  }
}